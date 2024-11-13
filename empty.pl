use strict;
use warnings;
use Text::CSV;

my $csv = Text::CSV->new({ binary => 1, eol => "\n" });

# Open a file for writing
open my $fh, ">", "output.csv" or die "Could not open file: $!";

# Open qhost command for reading
open my $qhost_fh, "-|", "qhost -q" or die "Could not open qhost: $!";
<$qhost_fh>;
<$qhost_fh>;
<$qhost_fh>;
my ($host, $cores, $MEMTOT, $MEMUSE, $queue, $used, $drained);  # Declare these outside the loop to use later

while (my $line = <$qhost_fh>) {
    my @line = split " ", $line;

    if ($line[1] eq "linux-x64") {
        ($host, $cores, $MEMTOT, $MEMUSE) = @line[0, 2, 4, 5];
    }

    if ($line[2] =~ /^(\d+)\/(\d+)\/(\d+)$/) {
        my $used = $2;

        my ($queue, $drained) = @line[0, 3];
        my @qdata = ($host, $cores, $MEMTOT, $MEMUSE, $queue, $used, $drained);
            
        # Write data to CSV
        $csv->print($fh, \@qdata);
    }
}

close $qhost_fh;
close $fh;
