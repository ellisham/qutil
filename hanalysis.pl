use strict;
use warnings;
use Text::CSV;

# Check if an argument was passed
my ($inputMonth, $inputFile);
if (@ARGV) {
    # Assign the first argument to a variable
    $inputFile = $ARGV[0];
} else {
    print "Please provide an input value.\n";
    exit;
}

# Check if the file exists and provide proper error message if not 
unless (-e "/project/scv/dugan/sge/data/$inputFile") {
    die "The input file '$inputFile' does not exist.\n";
}

my $csv = Text::CSV->new({ binary => 1, eol => "\n" });

open my $fh, ">", "$inputFile.csv" or die "Could not open file: $!";  # File to write to

    my $file_path = "/project/scv/dugan/sge/data/$inputFile";
    open(my $filehandle, '<', $file_path) or die "Could not open file '$file_path': $!";
    
    <$filehandle>; # skip header
    
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


close $fh;
