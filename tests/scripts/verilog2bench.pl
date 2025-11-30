#!/usr/bin/perl
# verilog2bench.pl - Convert Verilog to BENCH format for Atalanta ATPG
# Supports combinational and sequential circuits

use strict;
use warnings;

if (@ARGV != 2) {
    die "Usage: $0 <input.v> <output.bench>\n";
}

my ($input_file, $output_file) = @ARGV;

# Read input Verilog file
open(my $in, '<', $input_file) or die "Cannot open $input_file: $!\n";
my $content = do { local $/; <$in> };
close($in);

# Remove comments
$content =~ s/\/\/.*$//mg;  # Single-line comments
$content =~ s/\/\*.*?\*\///gs;  # Multi-line comments

# Parse module
my ($module_name, $module_ports, $module_body);
if ($content =~ /module\s+(\w+)\s*\((.*?)\);(.*?)endmodule/s) {
    $module_name = $1;
    $module_ports = $2;
    $module_body = $3;
} else {
    die "Cannot parse module in $input_file\n";
}

# Data structures
my %inputs = ();
my %outputs = ();
my %regs = ();
my %wires = ();
my @gates = ();
my @dffs = ();

# Parse ANSI-style port list (modern Verilog)
# Format: input a, input b, output y
if ($module_ports) {
    # Split by commas, but be careful of arrays
    my @port_decls = split(/,(?![^\[]*\])/, $module_ports);
    foreach my $decl (@port_decls) {
        $decl =~ s/^\s+|\s+$//g;
        # Parse: input [width] name or output reg [width] name
        if ($decl =~ /input\s+(?:wire\s+)?(?:\[.*?\]\s+)?(\w+)/) {
            $inputs{$1} = 1;
        } elsif ($decl =~ /output\s+reg\s+(?:\[.*?\]\s+)?(\w+)/) {
            $outputs{$1} = 1;
            $regs{$1} = 1;
        } elsif ($decl =~ /output\s+(?:wire\s+)?(?:\[.*?\]\s+)?(\w+)/) {
            $outputs{$1} = 1;
        }
    }
}

# Also parse old-style declarations in module body
# Parse inputs
while ($module_body =~ /input\s+(?:wire\s+)?(?:\[.*?\]\s+)?(\w+(?:\s*,\s*\w+)*)/g) {
    my $signals = $1;
    foreach my $sig (split(/\s*,\s*/, $signals)) {
        $sig =~ s/^\s+|\s+$//g;
        $inputs{$sig} = 1;
    }
}

# Parse outputs
while ($module_body =~ /output\s+(?:reg\s+|wire\s+)?(?:\[.*?\]\s+)?(\w+(?:\s*,\s*\w+)*)/g) {
    my $signals = $1;
    foreach my $sig (split(/\s*,\s*/, $signals)) {
        $sig =~ s/^\s+|\s+$//g;
        $outputs{$sig} = 1;
    }
}

# Parse output regs
while ($module_body =~ /output\s+reg\s+(?:\[.*?\]\s+)?(\w+(?:\s*,\s*\w+)*)/g) {
    my $signals = $1;
    foreach my $sig (split(/\s*,\s*/, $signals)) {
        $sig =~ s/^\s+|\s+$//g;
        $regs{$sig} = 1;
    }
}

# Parse standalone regs
while ($module_body =~ /^\s*reg\s+(?:\[.*?\]\s+)?(\w+(?:\s*,\s*\w+)*)/mg) {
    my $signals = $1;
    foreach my $sig (split(/\s*,\s*/, $signals)) {
        $sig =~ s/^\s+|\s+$//g;
        $regs{$sig} = 1;
    }
}

# Parse wires
while ($module_body =~ /wire\s+(?:\[.*?\]\s+)?(\w+(?:\s*,\s*\w+)*)/g) {
    my $signals = $1;
    foreach my $sig (split(/\s*,\s*/, $signals)) {
        $sig =~ s/^\s+|\s+$//g;
        $wires{$sig} = 1;
    }
}

# Parse gate primitives
while ($module_body =~ /(and|or|not|nand|nor|xor|xnor|buf)\s+\w+\s*\((.*?)\)\s*;/g) {
    my $gate_type = uc($1);
    my $ports = $2;
    $gate_type = "BUFF" if $gate_type eq "BUF";

    # Parse ports: first is output, rest are inputs
    my @port_list = split(/\s*,\s*/, $ports);
    my $output = shift @port_list;
    my @inputs = @port_list;

    push @gates, {
        type => $gate_type,
        output => $output,
        inputs => \@inputs
    };

    $wires{$output} = 1;
}

# Parse always blocks for flip-flops
while ($module_body =~ /always\s+@\s*\((.*?)\)\s+begin(.*?)end/gs) {
    my $sensitivity = $1;
    my $block = $2;

    # Check if this is a clocked always block
    my $clk_signal;
    if ($sensitivity =~ /posedge\s+(\w+)/ || $sensitivity =~ /negedge\s+(\w+)/) {
        $clk_signal = $1;
    } else {
        next;  # Skip non-clocked blocks
    }

    # Parse assignments in the block
    # Handle simple D flip-flop pattern: q <= d
    while ($block =~ /(\w+)\s*<=\s*([^;]+);/g) {
        my $q = $1;
        my $d_expr = $2;
        $d_expr =~ s/^\s+|\s+$//g;

        # Skip reset assignments (if rst)
        next if $d_expr =~ /1'b0|1'b1|2'b00|2'b0|4'b0000|4'b0/;

        # For simple assignments like q <= d or q <= d_next
        if ($d_expr =~ /^(\w+)$/) {
            my $d = $1;
            push @dffs, {
                q => $q,
                d => $d,
                clk => $clk_signal
            };
        }
        # Handle multi-bit registers with expressions
        elsif ($d_expr =~ /\{/) {
            # For shift registers: q <= {q[2:0], si}
            # Create individual DFFs for each bit
            if ($d_expr =~ /\{(.*?)\}/) {
                my $concat = $1;
                # For now, create a wire for the expression result
                my $wire_name = "${q}_next";
                $wires{$wire_name} = 1;
                push @dffs, {
                    q => $q,
                    d => $wire_name,
                    clk => $clk_signal
                };
            }
        }
        # Handle arithmetic like count + 1
        elsif ($d_expr =~ /(\w+)\s*\+\s*1/) {
            my $operand = $1;
            my $wire_name = "${q}_next";
            $wires{$wire_name} = 1;
            push @dffs, {
                q => $q,
                d => $wire_name,
                clk => $clk_signal
            };
        }
    }
}

# Generate BENCH output
open(my $out, '>', $output_file) or die "Cannot open $output_file: $!\n";

print $out "# $module_name\n";
print $out "# Generated by verilog2bench.pl from $input_file\n\n";

# Determine all clocks
my %clocks = ();
foreach my $dff (@dffs) {
    $clocks{$dff->{clk}} = 1;
}

# Output inputs (excluding clocks for sequential circuits)
foreach my $sig (sort keys %inputs) {
    # For sequential circuits, clocks are implicit, not listed as inputs
    next if exists $clocks{$sig};
    print $out "INPUT($sig)\n";
}

# For sequential circuits, add clocks as inputs
foreach my $clk (sort keys %clocks) {
    print $out "INPUT($clk)\n";
}

# Output outputs
foreach my $sig (sort keys %outputs) {
    print $out "OUTPUT($sig)\n";
}

print $out "\n";

# Output gates
foreach my $gate (@gates) {
    my $inputs_str = join(', ', @{$gate->{inputs}});
    print $out "$gate->{output} = $gate->{type}($inputs_str)\n";
}

# Output DFFs
foreach my $dff (@dffs) {
    # BENCH format for DFF: Q = DFF(D, CLK)
    # or Q = DFF(D) with implicit clock
    print $out "$dff->{q} = DFF($dff->{d})\n";
}

close($out);

print "Converted $input_file to $output_file\n";
print "  Inputs: ", scalar(keys %inputs), "\n";
print "  Outputs: ", scalar(keys %outputs), "\n";
print "  Gates: ", scalar(@gates), "\n";
print "  DFFs: ", scalar(@dffs), "\n";
