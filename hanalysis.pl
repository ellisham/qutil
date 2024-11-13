use strict;
use warnings;
use Text::CSV;

my $csv = Text::CSV->new({ binary => 1, eol => "\n" });

my @files = ("2401.h");

open my $fh, ">", "hfiles.csv" or die "Could not open file: $!";  # File to write to
foreach my $file (@files) {

    my $file_path = "/project/scv/dugan/sge/data/$file";
    open(my $filehandle, '<', $file_path) or die "Could not open file '$file_path': $!";
    
    <$filehandle>;
    
    my ($time, $host, $cores, $MEMTOT, $MEMUSE, $queue, $used, $drained);

    
    while (my $line = <$filehandle>) {
        my @line = split /\s+/, $line;


        if ($line[2] eq "linux-x64") {
            ($time, $host, $cores, $MEMTOT, $MEMUSE) = @line[0, 1, 3, 5, 6];
        }

        if ($line[3] =~ /^(\d+)\/(\d+)\/(\d+)$/) {            
            
            $used = $2;  

            ($queue, $drained) = @line[1, 4];

            my @qdata = ($time, $host, $cores, $MEMTOT, $MEMUSE, $queue, $used, $drained);
            $csv->print($fh, \@qdata) or die "Failed to print to CSV: " . $csv->error_diag();
    }
    }

    close $filehandle;
}

close $fh;
