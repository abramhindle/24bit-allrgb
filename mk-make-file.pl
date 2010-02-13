#!/usr/bin/perl
use strict;
my ($img,$blocksize,$slices) = @ARGV;
my $n = 4096 / $slices ;
my @all = ();
print "all: $img.out.png$/$/";
for my $x (0..($slices-1)) {
	my $file = "$img.$blocksize.".($x*$n).".".$n.".png";
	print "$file:$/";
	print "\t./permuter $img $blocksize ".($x * $n)." $n$/";
	push @all, $file;
}
print "$img.out.png: ".join(" ",@all).$/;
print "\tconvert -append ".join(" ",@all)." $img.out.png$/";
