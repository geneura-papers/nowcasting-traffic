#!/usr/bin/env perl

use strict;
use warnings;

use File::Slurp::Tiny qw(read_lines write_file);
use DateTime;

use v5.14;

my $id = shift || "1011";

my %count_dgt = process_file( "DGT_$id.csv");
my %count_petra = process_file( "PETRA_$id.csv");

for my $date ( keys %count_dgt ) {
  next if !$count_petra{$date};
  my ($year,$mon,$day,$hour) = ($date =~ /(\d+)-(\d+)-(\d+)\s+(\d)/);
  my $dt = DateTime->new(year => $year, month => $mon, day => $day );
  say $dt->day_name,",$mon,$hour,$count_petra{$date},$count_dgt{$date}";
}

sub process_file {
  my $file= shift;
  my @content = read_lines( $file );
  my %count;
  for my $c (@content) {
    chomp($c);
    my ($date, $foo, $count) = split(/[,;]/, $c );
    $count{$date} = $count;
  }
  return %count;
}
