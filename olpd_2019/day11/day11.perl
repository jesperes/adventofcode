#!/usr/bin/perl
use strict;
use warnings;

sub make_intcode {
    my @prog = shift;
    my $intcode = {
        prog => \@prog,
        pc => 42,
        relbase => 0,
        last_opcode => 0,
        input_pending => 0,
        input => 0,
        output_pending => 0,
        output => 0
    };
    return $intcode;
}

sub read_file {
    open(my $in, "<", shift) or die "Open failed: $!";
    return <$in>;
}

sub execute {
    my $intcode = shift;
    print "$intcode->{pc}\n";
}

my @prog = split(",", "1,9,10,3,2,3,11,0,99,30,40,50");
my $p = make_intcode @prog;
execute $p;
