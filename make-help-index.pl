#! perl -w
use strict;

sub unhtmlize {
  my ($str) = @_;
  $str =~ s/&lt;/</g;
  $str =~ s/&gt;/>/g;
  $str =~ s/&mdash;/-/g;
  $str =~ s|\\|\\\\|g;
  return $str;
}

sub scan_command_index {
  my ($index_file) = @_;
  open INFILE, $index_file or die "Cannot open file $index_file ($!)";
  my $result = {};
  my $line;
  while ($line = <INFILE>) {
    if ($line =~ m|<tr><td></td><td valign="top"><a href="([^"]+)"><code>([^<]+)</code></a></td><td valign="top"><a href="([^"]+)">([^<]+)</a></td></tr>|) {
      my $entry = $2;
      my $entry_link = $1;
      my $section = $4;
      my $section_link = $3;
      $entry = unhtmlize ($entry);
      $section = unhtmlize ($section);
      my $entry_data = {};
      $entry_data->{link} = $entry_link;
      $entry_data->{section} = $section;
      $entry_data->{section_link} = $section_link;
      push @{$result->{$entry}}, $entry_data;
    }
  }
  close INFILE;
  return $result;
}

sub write_elisp_index {
  my ($output_file, $index, $url_base) = @_;
  open OUTFILE, ">$output_file" or die "Cannot write file '$output_file' ($!)";
  print OUTFILE "(defconst lyqi:help-url-base \"$url_base\")\n\n";
  print OUTFILE "(defconst lyqi:help-index\n  '(";
  my $first_entry = 1;
  foreach my $entry (sort keys %$index) {
    print OUTFILE "\n    " unless $first_entry;
    print OUTFILE "(\"$entry\" ";
    my $first_data = 1;
    foreach my $entry_data (reverse @{$index->{$entry}}) {
      print OUTFILE "\n       " unless $first_data;
      print OUTFILE "(\"", $entry_data->{link}, "\" \"", $entry_data->{section}, "\" \"", $entry_data->{section_link}, "\")";
      $first_data = 0;
    }
    print OUTFILE ")";
    $first_entry = 0;
  }
  print OUTFILE "))\n\n";
  my $feature = $output_file;
  $feature =~ s|(.*/)?([^/]+)\.el|$2|;
  print OUTFILE "(provide '$feature)\n";
  close OUTFILE;
}

my ($index_file, $url_base, $output_file) = @ARGV;

my $index = scan_command_index ($index_file);
write_elisp_index ($output_file, $index, $url_base);
