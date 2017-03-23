#!c:\Perl\bin\perl.exe
use strict;
use Tk;
require Tk::DateEntry;
require Tk::JComboBox;
#use Data::Dumper;
require Tk::BMatchEntry;
require Tk::Labelled;
require Tk::NumEntry;
###################################################
# @Coder: kolunya
# @Purpose: Custom graphical DBI to facilitate data entry for USBC Awards
# @Usage: $0
# *Considerations: Two input .csv files in same directory 
# Changelog:
#  .2a: Font change (by request)
#  .3a: Commit button focus color; 3-digit max then focusNext for game fields; <ENTER> moves to next field.
#  .4a: Killing diagnostic output to STDERR from default.
###################################################

my @z=("00" .. "99");
my $diagmsg=0; 	#By default, do not output diagnostic messages to STDERR

sub churnfile_csv() {
	my($file)=@_;
	if (! -f "$file") {
		print STDERR "File does not exist: ".$file."\n" if $diagmsg; 
		return 0;
	}
	
	#############
	# 1. Extract field names from top line, assume first field is key
	# 2. Store remaining lines accordingly
	#############
	open(FILENAME,"<$file") || print STDERR "Couldn't open file to read: $!\n" && return 0;
	my @contents=<FILENAME>;
	close FILENAME;
	my $topline=shift @contents;
	$topline=~s/\015?\012//g;
	my @fieldlist=split ',',$topline;
	my %table=(file=>$file, fieldlist=>\@fieldlist, array=>\@contents, db=>{});
	for my $line (@contents) {
		$line=~s/\015?\012//g;
		my @values=split ',',$line;
		my $key=$values[0];
		$table{db}->{$key}={key=>$key};
		for my $i (0..$#values) {
			$table{db}->{$key}->{$fieldlist[$i]}=$values[$i];
			#next unless $file=~/league/i;
			#print "DIAG: \$table{db}->{$key}->{$fieldlist[$i]}=$values[$i];\n";
		}
		if ((exists($table{db}->{$key}->{Last})) && (exists($table{db}->{$key}->{First}))) {
			$table{db}->{$key}->{fullname}="$table{db}->{$key}->{Last}, $table{db}->{$key}->{First}";
		}
	}	
	return \%table;
}

sub pourfile_csv() {
	my $tableref=shift @_ || print STDERR "No hashref provided.  Fayling." && exit 0;
	my $file=shift @_ || $tableref->{file};
	if (! -f "$file") {
		print STDERR "File does not exist: ".$file.".  Creating anew.\n";
	} else {
		print STDERR "Over-writing file: $file\n";
	}
	open(FILENAME,">$file") || print STDERR "Couldn't open file to write: $!\n" && return 0;
	my @fieldlist=@{$tableref->{fieldlist}};
	print FILENAME ((join ',',@fieldlist)."\n");
	#print ((join ',',@fieldlist)."\n");
	for my $record (values %{$tableref->{db}}) {
		my @values;
		for my $field (@fieldlist) {
			push @values,$record->{$field};
		}
		print FILENAME ((join ',',@values)."\n");
		#print "$record->{key} REC: ";
		#print ((join ',',@values)."\n");
	}
	close(FILENAME);
	return 1;
}

sub listof() {
	my($tableref,$field)=@_;
	my $db=$tableref->{db};
	my %lists=();
	for my $key (keys %{$db}) {
		$lists{$db->{$key}->{$field}}=[] unless exists $lists{$db->{$key}->{$field}};
		push @{$lists{$db->{$key}->{$field}}},$key;
	}
	return \%lists;
}

die "No point continuing without required input file..." unless my $people=&churnfile_csv("PeopleQuery.csv");
die "No point continuing without required input file..." unless my $league=&churnfile_csv("LeagueQuery.csv");
#############
# Create some indexes to make my life easier...
#############
my $centers=&listof($league,"Center_Name");
my @centers=keys %{$centers};
#for my $key (keys %{$centers}) {
#	print "$key -- ( @{$centers->{$key}} )\n";
#}
my @leagues=sort {$a cmp $b} (keys %{$league->{db}});
my $lfnames=&listof($people,"fullname");
my @fullnames=sort {$a cmp $b} (keys %{$lfnames});
##Field naming convention here: Upper Case first letter means fixed data entry, lower case first letter means save to file but recalc in-program on re-display.
my @fieldlist=("Awardkey","USBCID","last","first","gender","type","Bowled_In","League_Name","center_name","season","bowlingyear","yearbookabbrev","Date_Bowled","Game1","Game2","Game3","series","g1_225","g2_225","g3_225","g1_250","g2_250","g3_250","g1_275","g2_275","g3_275","s600","s650","s700","reg","srm","srw");


############################
# Interface globals (KEEP TO A MINIMUM, bub)
############################
my $alertmessage="Welcome to the Awards Database v0.3a";
my $needsave=0;

# Read in our database, and if it doesn't exist, create a blank DB programmatically.
my $temphash=&churnfile_csv("award_db.csv");
if (! $temphash) {
	$temphash={file=>"award_db.csv",fieldlist=>\@fieldlist,db=>{}};
	$needsave=1;
}
my %hash=%{$temphash};
my @recsort=sort { $a <=> $b } keys %{$hash{db}};
my $recpoint=scalar(@recsort);

sub genrec() {
	# Allow defaults; accept key/value pairs. First param must be hashref.
	my $rec = shift || return 0;
	my %pars=(@_);
	for my $fieldname (@fieldlist, "key") {
		if (exists $rec->{$fieldname}) {
			print STDERR "Retaining field $fieldname value $rec->{$fieldname}\n" if $diagmsg;
		} elsif (exists $pars{$fieldname}) {
			print STDERR "Accepting default val for field $fieldname value $pars{$fieldname}\n" if $diagmsg;
			$rec->{$fieldname}=$pars{$fieldname};
		} else {
			$rec->{$fieldname}="";
		}
	}
	$rec->{fullname}="";
	&recalc_all($rec);
}

sub clearrec() {
	my $rec = shift || return 0;
	my %pars=(@_);
	for my $fieldname (@fieldlist, "key") {
		if (exists $pars{$fieldname}) {
			print STDERR "Accepting default val for field $fieldname value $pars{$fieldname}\n" if $diagmsg;
			$rec->{$fieldname}=$pars{$fieldname};
		} else {
			$rec->{$fieldname}="";
		}
	}
	$rec->{fullname}="";
	&recalc_all($rec);
}

sub recalc_bowler() {
	my $rec = shift || return 0;
	return 0 unless $rec->{USBCID};
	print STDERR "USBCID defined, '$rec->{USBCID}'\n" if $diagmsg;
	my $key=$rec->{USBCID};
	if (exists $people->{db}->{$key}) {
		my $l=$people->{db}->{$key};
		print STDERR "Found record $l->{Last}, $l->{First}: $l->{Gender} $l->{Type}.\n" if $diagmsg;
		$rec->{first}=$l->{First};
		$rec->{last}=$l->{Last};
		$rec->{gender}=$l->{Gender};
		$rec->{type}=$l->{Type};
		$rec->{reg}=""; $rec->{srm}=""; $rec->{srw}="";
		if ($rec->{gender} eq "M" && $rec->{type} eq "Regular") {
			print STDERR "DIAG reg == $rec->{gender} $rec->{type}\n" if $diagmsg;
			$rec->{reg}="1";
		}
		if ($rec->{gender} eq "F" || $rec->{type} eq "Senior") {
			print STDERR "DIAG srm == $rec->{gender} $rec->{type}\n" if $diagmsg;
			$rec->{srm}="1";
		}
		if ($rec->{gender} eq "F" && $rec->{type} eq "Senior") {
			print STDERR "DIAG srw == $rec->{gender} $rec->{type}\n" if $diagmsg;
			$rec->{srw}="1";
		}
		return 1;
	}
	return 0;
}
my $leaguechoices=\@leagues;

sub recalc_league() {
	my $rec = shift || return 0;
	return 0 unless $rec->{League_Name};
	my $key=$rec->{League_Name};
	if (exists $league->{db}->{$key}) {
		my $l=$league->{db}->{$key};
		print STDERR "Found LEAGUE record $l->{League_Name} at $l->{Center_Name} ($l->{YearbookAbbrev}) for $l->{BowlingYear} $l->{Season}\n" if $diagmsg;
		$rec->{Bowled_In}="L" unless $rec->{Bowled_In};
		$rec->{center_name}=$l->{Center_Name};
		$rec->{season}=$l->{Season};
		$rec->{yearbookabbrev}=$l->{YearbookAbbrev};
		$rec->{bowlingyear}=$l->{BowlingYear};
		return 1;
	}
	return 0;
}

sub recalc_stats() {
	my $rec = shift || return 0;
	return 0 unless $rec->{Date_Bowled};
	if (($rec->{Game1})&&($rec->{Game2})&&($rec->{Game3})) {
		$rec->{series}=$rec->{Game1} + $rec->{Game2} + $rec->{Game3};
		print STDERR "RECALC: series=$rec->{series}\n" if $diagmsg;
		$rec->{s600}=""; $rec->{s650}=""; $rec->{s700}="";
		if ($rec->{srw} && $rec->{series}>=600 && $rec->{series}<650) { $rec->{s600}="1" }
		if ($rec->{srm} && $rec->{series}>=650 && $rec->{series}<700) { $rec->{s650}="1" }
		if ($rec->{series}>=700 && $rec->{series}<800) { $rec->{s700}="1" }
	}
	for my $f (qw(g1_225 g2_225 g3_225 g1_250 g2_250 g3_250 g1_275 g2_275 g3_275)) { $rec->{$f}=""; }
	if ($rec->{srw} && $rec->{Game1}>=225 && $rec->{Game1}<250) { $rec->{g1_225}="1" }
	if ($rec->{srw} && $rec->{Game2}>=225 && $rec->{Game2}<250) { $rec->{g2_225}="1" }
	if ($rec->{srw} && $rec->{Game3}>=225 && $rec->{Game3}<250) { $rec->{g3_225}="1" }
	if ($rec->{srm} && $rec->{Game1}>=250 && $rec->{Game1}<275) { $rec->{g1_250}="1" }
	if ($rec->{srm} && $rec->{Game2}>=250 && $rec->{Game2}<275) { $rec->{g2_250}="1" }
	if ($rec->{srm} && $rec->{Game3}>=250 && $rec->{Game3}<275) { $rec->{g3_250}="1" }
	if ($rec->{Game1}>=275 && $rec->{Game1}<300) { $rec->{g1_275}="1" }
	if ($rec->{Game2}>=275 && $rec->{Game2}<300) { $rec->{g2_275}="1" }
	if ($rec->{Game3}>=275 && $rec->{Game3}<300) { $rec->{g3_275}="1" }
}

sub recalc_all() {
	my $rec = shift || return 0;
	#Pass record ref to each calc function
	&recalc_bowler($rec);
	&recalc_league($rec);
	&recalc_stats($rec);
}

sub setkey() {
	return time();
}

my $blank={};
#&genrec($blank,key=>"random_awardkey_insert_later",Awardkey=>"random_awardkey_insert_later",USBCID=>"2930-12018",League_Name=>"Friday Mixed",Date_Bowled=>"1/1/2012",Game1=>275,Game2=>250,Game3=>150);
#$hash{db}->{"random_awardkey_insert_later"}={};
#&genrec($hash{db}->{"random_awardkey_insert_later"},%{$blank});
&clearrec($blank);
my $currentrec={};
%{$currentrec}=%{$blank};

############################
# Now we build our interface
############################
my $mw=MainWindow->new;
$mw->configure(-title=>'Awards Database');
$mw->geometry("1100x280+50+50");
my $F=$mw->Frame(-background=>'light grey')->pack(-side=>'top',-fill=>'both', -expand=>1);

my $msglabel=$F->Label(-textvariable=>\$alertmessage, -font=>'times 12 normal', -background=>'light grey')->pack(-side=>'top',-fill=>'x');
	sub setalert() {
		my $message=shift || "";
		my $color=shift || 'light grey';
		$alertmessage=$message;
		$msglabel->configure(-background=>$color);
	}
my $buttonF=$F->Frame(-background=>'light grey')->pack(-side=>'bottom',-fill=>'x');
my $formF=$F->Frame(-background=>'light grey')->pack(-side=>'top',-fill=>'both', -expand=>1);

## Right-side program execution buttons
my $exitB=$buttonF->Button(-font=>'times 12 normal', -takefocus=>0, -text=>'Exit', -command=>sub { exit; })->pack(-side=>'right');
my $saveB=$buttonF->Button(-font=>'times 12 normal', -takefocus=>0, -text=>'Save')->pack(-side=>'right');
	sub needsave() {
		my $need=shift || 0;
		$needsave=$need;
		if ($needsave) {
			$saveB->configure(-background=>'red');
		} else {
			$saveB->configure(-background=>'light grey');
		}
		return 1;
	}
$saveB->configure(-command=>sub { &pourfile_csv(\%hash) && &needsave(0) && &setalert("Finished saving awards database.","green"); });
&needsave($needsave);
my $resetB;

## Left-side record transit buttons
my $prevrecB=$buttonF->Button(-font=>'times 12 normal', -takefocus=>0, -text=>"<<", -command=>sub {
		#Load previous record in list, sorted numerically by key, discarding any changes current in displayed record
		if ($recpoint>0) {
			&clearrec($currentrec);
			$recpoint=$recpoint-1;
			my $key=$recsort[$recpoint];
			&clearrec($currentrec,%{$hash{db}->{$key}});
			&setalert("Viewing record $key\.","light grey");
		}
	})->pack(-side=>'left');
my $nextrecB=$buttonF->Button(-font=>'times 12 normal', -takefocus=>0, -text=>">>", -command=>sub {
		#Load next record in list, sorted numerically by key, discarding any changes current in displayed record, blanking if this is latest record
		&clearrec($currentrec);
		if ($recpoint<$#recsort) {
			$recpoint=$recpoint+1;
			my $key=$recsort[$recpoint];
			&clearrec($currentrec,%{$hash{db}->{$key}});
			&setalert("Viewing record $key\.","light grey");
		} else {
			&setalert("Advanced to end of database.","yellow");
		}
	})->pack(-side=>'left');

## Middle-side report output buttons
my $highscoreReportB=$buttonF->Button(-font=>'times 10 normal', -takefocus=>0, -text=>"High Scores\nReport", -command=>sub {
		# One CSV file for each of men and women bowlers
		# Going to work this by record and file accordingly, then output at end, rather than building in bowler sort order and creating output as we go.
		my(%regm,%regw);
		# Another way, by bowler: my $bowlerawards=&listof(\%hash,"Awardkey");
		for my $recordkey (keys %{$hash{db}}) {
			my $rec=$hash{db}->{$recordkey};
			my $key=$rec->{USBCID};
			my $work;
			#if ($rec->{srw}) { $work=\%srw; }
			if ($rec->{gender}=~/m/i) { $work=\%regm; }
			#if ($rec->{reg}) { $work=\%regm; }
			if ($rec->{gender}=~/f/i) { $work=\%regw; }
			
			$work->{$key}=[] unless exists $work->{$key};
			#if ($rec->{srw} && $rec->{series}>=600) { push @{$work->{$key}},$rec->{series}; }
			if ($rec->{gender}=~/f/i && $rec->{series}>=650) { push @{$work->{$key}},$rec->{series}; }
			if ($rec->{gender}=~/m/i && $rec->{series}>=700) { push @{$work->{$key}},$rec->{series}; }

			for my $f (qw(Game1 Game2 Game3)) { 
				#if ($rec->{srw} && $rec->{$f}>=225) { push @{$work->{$key}},$rec->{$f}; }
				if ($rec->{gender}=~/f/i && $rec->{$f}>=250) { push @{$work->{$key}},$rec->{$f}; }
				if ($rec->{gender}=~/m/i && $rec->{$f}>=275) { push @{$work->{$key}},$rec->{$f}; }
			}
		}
		open(HIGHOUT,">highscore_men.csv") || die "FAYL: $!";
		print HIGHOUT "NAME,HIGHSCORES\n";
		for my $usbcid (sort { $people->{db}->{$a}->{Last} cmp $people->{db}->{$b}->{Last} || $people->{db}->{$a}->{First} cmp $people->{db}->{$b}->{First} } keys %regm) {
			next unless scalar(@{$regm{$usbcid}})>0;
			print HIGHOUT (qq("$people->{db}->{$usbcid}->{Last}, $people->{db}->{$usbcid}->{First}",).(join ',',sort { $b <=> $a } @{$regm{$usbcid}})."\n");
		}
		close(HIGHOUT);
		open(HIGHOUT,">highscore_women.csv") || die "FAYL: $!";
		print HIGHOUT "NAME,HIGHSCORES\n";
		for my $usbcid (sort { $people->{db}->{$a}->{Last} cmp $people->{db}->{$b}->{Last} || $people->{db}->{$a}->{First} cmp $people->{db}->{$b}->{First} } keys %regw) {
			next unless scalar(@{$regw{$usbcid}})>0;
			print HIGHOUT (qq("$people->{db}->{$usbcid}->{Last}, $people->{db}->{$usbcid}->{First}",).(join ',',sort { $b <=> $a } @{$regw{$usbcid}})."\n");
		}
		close(HIGHOUT);
		&setalert("High Scores reports printed.");
	})->pack(-side=>'top');

my $commitB;
my $datebowledE;

sub reccheck() {
	my $rec=shift;
	if ($currentrec->{USBCID}) {
		$currentrec->{fullname}="";
	}
	if (! $currentrec->{key}) {
		if ($currentrec->{USBCID} && $currentrec->{Date_Bowled} && $currentrec->{League_Name}) {
			my $key=&setkey();
			$currentrec->{key}=$key;
			$currentrec->{Awardkey}=$key;
			$commitB->configure(-state=>'normal');
		} else {
			$commitB->configure(-state=>'disabled');
		}
	} else {
		$commitB->configure(-state=>'normal');
	}
	# Now also checking for dates out of range
	if ($rec->{Date_Bowled}=~m#(\d+)/(\d+)/(\d+)#) {
		my($m,$d,$y)=($1,$2,$3);
		$y=1900+$y if $y<200;
		$y=$y+100 if $y<1970;
		$rec->{Date_Bowled}="$z[$m]/$z[$d]/$y";
		if (($m>5 && $y>2012) || ($m<6 && $y<2013)) {
			&setalert("Date '$rec->{Date_Bowled}' out of recommended range!","orange");
			$datebowledE->focus();
			return 0;
		} elsif ($alertmessage=~/^Date/) {
			&setalert();
			return 1;
		}
	}
	return 1;
}

my $leagueF=$formF->Frame(-background=>'light grey')->pack(-side=>'top',-fill=>'x');
my $leaguenameME;
sub set_leaguelist() {
	if ($currentrec->{center_name} eq "") {
		$leaguenameME->configure(-choices=>\@leagues);
	}
	$leaguenameME->configure(-choices=>$centers->{$currentrec->{center_name}}) if exists $centers->{$currentrec->{center_name}};
	if ($currentrec->{League_Name} && (! grep /$currentrec->{League_Name}/,@{$centers->{$currentrec->{center_name}}})) {
		$currentrec->{League_Name}="";
		&recalc_league($currentrec);
	}
}

my $fullnameME;
my $usbcidME;
sub set_usbcids() {
	if ($currentrec->{fullname} eq "") {
		$usbcidME->configure(-choices=>[keys %{$people->{db}}]);
	} else {
		if (exists $lfnames->{$currentrec->{fullname}}) {
			$usbcidME->configure(-choices=>$lfnames->{$currentrec->{fullname}});
			if (scalar(@{$lfnames->{$currentrec->{fullname}}}) == 1) {
				$currentrec->{USBCID}=$lfnames->{$currentrec->{fullname}}->[0];
				&recalc_bowler($currentrec);
			}
		}
	}
}

$leaguenameME=$leagueF->BMatchEntry(
	-font=>'times 12 normal', 
	-label=>"League", 
	-textvariable=> \$currentrec->{League_Name}, 
	-choices=>$leaguechoices, 
	-ignorecase=>1, 
	-entercmd=>sub { &recalc_league($currentrec); &reccheck($currentrec); $fullnameME->focus(); },
	-tabcmd=>sub { &recalc_league($currentrec); &reccheck($currentrec); $fullnameME->focus(); },
	-browsecmd=>sub { &recalc_league($currentrec); &reccheck($currentrec); $fullnameME->focus(); },
	-labelPack=>[-side=>'left', -fill=>'none', -expand=>0] 
);

my $centerME=$leagueF->BMatchEntry(
	-font=>'times 12 normal', 
	-label        => "Center", 
	-textvariable => \$currentrec->{center_name}, 
	-choices      => \@centers, 
	-ignorecase   => 1, 
	-entercmd=>\&set_leaguelist,
	-tabcmd=>\&set_leaguelist,
	-browsecmd=>\&set_leaguelist,
	-labelPack=>[-side=>'left', -fill=>'none', -expand=>0] 
);

&set_leaguelist;

my $bowlerF=$formF->Frame(-background=>'light grey')->pack(-side=>'top',-fill=>'x');
$fullnameME=$bowlerF->BMatchEntry(
	-font=>'times 12 normal', 
	-label=>"Name Lookup", 
	-textvariable=> \$currentrec->{fullname}, 
	-choices=>\@fullnames, 
	-ignorecase=>1, 
	-entercmd=>sub { &set_usbcids; $usbcidME->focus(); },
	-tabcmd=>sub { &set_usbcids; $usbcidME->focus(); },
	-browsecmd=>sub { &set_usbcids; $usbcidME->focus(); },
	-labelPack=>[-side=>'left', -fill=>'none', -expand=>0] 
);

$usbcidME=$bowlerF->BMatchEntry(
	-font=>'times 12 normal', 
	-label=>"USBCID", 
	-textvariable=> \$currentrec->{USBCID}, 
	-choices=>[keys %{$people->{db}}], 
	-ignorecase=>1, 
	-entercmd=>sub { &recalc_bowler($currentrec); &reccheck($currentrec); $datebowledE->focus(); },
	-tabcmd=>sub { &recalc_bowler($currentrec); &reccheck($currentrec); $datebowledE->focus(); },
	-browsecmd=>sub { &recalc_bowler($currentrec); &reccheck($currentrec); $datebowledE->focus(); },
	-labelPack=>[-side=>'left', -fill=>'none', -expand=>0] 
);

$leaguenameME->bind('<FocusOut>',sub { &reccheck($currentrec); }); #$usbcidME->focusForce(); $usbcidME->focus(); });

my $game1E;
my $entryF=$formF->Frame(-background=>'light grey')->pack(-side=>'top',-fill=>'x');
$datebowledE=$entryF->DateEntry(-font=>'times 12 normal', -textvariable=>\$currentrec->{Date_Bowled}, -formatcmd=>sub { &reccheck($currentrec) && $game1E->focus(); return "$z[$_[1]]/$z[$_[2]]/$_[0]"; } );

$usbcidME->bind('<FocusOut>',sub { &reccheck($currentrec); }); #$datebowledE->focusForce(); $datebowledE->focus(); });

$game1E=$entryF->Labelled("NumEntry", -font=>'times 12 normal', -label=>"Game 1", -buttons=>0, -textvariable=>\$currentrec->{Game1}, -width=>4, -labelPack=>[-side=>'left', -fill=>'none', -expand=>0] );

$datebowledE->bind('<KP_Enter>',sub { &reccheck($currentrec); $game1E->focus(); }); #$game1E->focusForce(); });
$datebowledE->bind('<FocusOut>',sub { &reccheck($currentrec); }); #$game1E->focusForce(); $game1E->focus(); });
$datebowledE->bind('<Return>',sub { &reccheck($currentrec); $game1E->focus(); }); #$game1E->focusForce(); });

my $game2E=$entryF->Labelled("NumEntry", -font=>'times 12 normal', -label=>"Game 2", -buttons=>0, -textvariable=>\$currentrec->{Game2}, -width=>4, -labelPack=>[-side=>'left', -fill=>'none', -expand=>0] );
$datebowledE->bind('<KP_Enter>',sub { &reccheck($currentrec); $game1E->focus(); }); #$game1E->focusForce(); });
$datebowledE->bind('<Return>',sub { &reccheck($currentrec); $game1E->focus(); }); #$game1E->focusForce(); });
my $game3E=$entryF->Labelled("NumEntry", -font=>'times 12 normal', -label=>"Game 3", -buttons=>0, -textvariable=>\$currentrec->{Game3}, -width=>4, -labelPack=>[-side=>'left', -fill=>'none', -expand=>0] );
my $seriesROE=$entryF->Labelled("NumEntry", -font=>'times 12 normal', -label=>"Series", -buttons=>0, -textvariable=>\$currentrec->{series}, -width=>4, -state=>'disabled', -labelPack=>[-side=>'left', -fill=>'none', -expand=>0] );
$game1E->bind('<KeyPress>',sub { &recalc_stats($currentrec); if (length($currentrec->{Game1})==3) { $game2E->focus(); } });
$game1E->bind('<KP_Enter>',sub { &recalc_stats($currentrec); $game2E->focus(); });
$game1E->bind('<Return>',sub { &recalc_stats($currentrec); $game2E->focus(); });
$game2E->bind('<KeyPress>',sub { &recalc_stats($currentrec); if (length($currentrec->{Game2})==3) { $game3E->focus(); } });
$game2E->bind('<KP_Enter>',sub { &recalc_stats($currentrec); $game3E->focus(); });
$game2E->bind('<Return>',sub { &recalc_stats($currentrec); $game3E->focus(); });
$game3E->bind('<KeyPress>',sub { &recalc_stats($currentrec); if (length($currentrec->{Game3})==3) { $commitB->focus(); } });
$game3E->bind('<KP_Enter>',sub { &recalc_stats($currentrec); $commitB->focus(); });
$game3E->bind('<Return>',sub { &recalc_stats($currentrec); $commitB->focus(); });
my $statsF=$formF->Frame(-background=>'light grey')->pack(-side=>'top',-fill=>'x');
my $formbuttonsF=$formF->Frame(-background=>'light grey')->pack(-side=>'top',-fill=>'x');

$leaguenameME->pack(-side=>'left', -expand=>0);
$centerME->pack(-side=>'left', -expand=>0);
$fullnameME->pack(-side=>'left', -expand=>0);
$usbcidME->pack(-side=>'left', -expand=>0);
$datebowledE->pack(-side=>'left', -expand=>0);
$game1E->pack(-side=>'left', -expand=>0);
$game2E->pack(-side=>'left', -expand=>0);
$game3E->pack(-side=>'left', -expand=>0);

$commitB=$formbuttonsF->Button(
	-font=>'times 12 normal', 
	-text=>'Commit Record', 
	-command=>sub {
		#Push currentrec to %hash && add to @recsort if appropriate
		if (! exists $hash{db}->{$currentrec->{key}}) {
			$hash{db}->{$currentrec->{key}}={key=>$currentrec->{key}};
			push @recsort,$currentrec->{key};
			$recpoint=scalar(@recsort);
			&clearrec($hash{db}->{$currentrec->{key}},%{$currentrec});
			#&needsave(1)
			&needsave(1);
			#clear record (??) but leave current center/league/date_bowled as defaults
			&clearrec($currentrec,League_Name=>$currentrec->{League_Name},Date_Bowled=>$currentrec->{Date_Bowled});
			#Set focus back to usbcidME ###kwedit 130702 - Now to $fullnameME
			#Set commitB state=>'disabled'
			&setalert("Database has been changed.");
			&reccheck($currentrec); 
			$fullnameME->focusForce(); 
			$fullnameME->focus();
		} else {
			&clearrec($hash{db}->{$currentrec->{key}},%{$currentrec});
			#&needsave(1)
			&needsave(1);
			#clear record (??) but leave current center/league/date_bowled as defaults
			&setalert("Database has been changed.");
			&reccheck($currentrec); 
		}
	},
	-activebackground=>'yellow',
	-state=>'disabled'
)->pack(-side=>'right');
$commitB->bind('<FocusIn>',sub { $commitB->configure(-background=>"orange") unless $commitB->cget("-state") eq "disabled"; });
$commitB->bind('<FocusOut>',sub { $commitB->configure(-background=>"light grey"); });

my $bowledinROE=$leagueF->Labelled("Entry", -background=>"light grey", -font=>'times 12 normal', -label=>"L/T",-textvariable=>\$currentrec->{Bowled_In}, -width=>5, -takefocus=>0, -labelPack=>[-side=>'left', -fill=>'none', -expand=>0] );
my $seasonROE=$leagueF->Labelled("Entry", -background=>"light grey", -font=>'times 12 normal', -label=>"Season",-textvariable=>\$currentrec->{season}, -width=>5, -takefocus=>0, -labelPack=>[-side=>'left', -fill=>'none', -expand=>0] );
my $yearROE=$leagueF->Labelled("Entry", -background=>"light grey", -font=>'times 12 normal', -label=>"Year",-textvariable=>\$currentrec->{bowlingyear}, -width=>5, -takefocus=>0, -labelPack=>[-side=>'left', -fill=>'none', -expand=>0] );

my $lastROE=$bowlerF->Labelled("Entry", -background=>"light grey", -font=>'times 12 normal', -label=>"Last",-textvariable=>\$currentrec->{last}, -takefocus=>0, -labelPack=>[-side=>'left', -fill=>'none', -expand=>0] );
my $firstROE=$bowlerF->Labelled("Entry", -background=>"light grey", -font=>'times 12 normal', -label=>"First",-textvariable=>\$currentrec->{first}, -width=>15, -takefocus=>0, -labelPack=>[-side=>'left', -fill=>'none', -expand=>0] );
my $genderROE=$bowlerF->Labelled("Entry", -background=>"light grey", -font=>'times 12 normal', -label=>"gender",-textvariable=>\$currentrec->{gender}, -width=>3, -takefocus=>0, -labelPack=>[-side=>'left', -fill=>'none', -expand=>0] );
my $typeROE=$bowlerF->Labelled("Entry", -background=>"light grey", -font=>'times 12 normal', -label=>"Type",-textvariable=>\$currentrec->{type}, -width=>7, -takefocus=>0, -labelPack=>[-side=>'left', -fill=>'none', -expand=>0] );

$bowledinROE->pack(-side=>'left', -expand=>0);
$seasonROE->pack(-side=>'left', -expand=>0);
$yearROE->pack(-side=>'left', -expand=>0);
$lastROE->pack(-side=>'left', -expand=>0);
$firstROE->pack(-side=>'left', -expand=>0);
$genderROE->pack(-side=>'left', -expand=>0);
$typeROE->pack(-side=>'left', -expand=>0);
$seriesROE->pack(-side=>'left', -expand=>0);

my $gcol=$statsF->Frame(-background=>'light grey')->pack(-side=>'left',-fill=>'both', -expand=>1);
my $g225F=$gcol->Frame(-background=>'light grey')->pack(-side=>'top',-fill=>'x');
my $g1225CB=$g225F->Checkbutton(-takefocus=>0, -state=>'disabled', -variable=>\$currentrec->{g1_225}, -onvalue=>"1", -offvalue=>"")->pack(-side=>'left');
my $g2225CB=$g225F->Checkbutton(-takefocus=>0, -state=>'disabled', -variable=>\$currentrec->{g2_225}, -onvalue=>"1", -offvalue=>"")->pack(-side=>'left');
my $g3225CB=$g225F->Checkbutton(-takefocus=>0, -state=>'disabled', -variable=>\$currentrec->{g3_225}, -onvalue=>"1", -offvalue=>"")->pack(-side=>'left');
my $g225L=$g225F->Label(-anchor=>'w', -font=>'arial 12 normal', -background=>'light grey', -text=>"225 Game")->pack(-side=>'left',-fill=>'x', -expand=>1);
my $g250F=$gcol->Frame(-background=>'light grey')->pack(-side=>'top',-fill=>'x');
my $g1250CB=$g250F->Checkbutton(-takefocus=>0, -state=>'disabled', -variable=>\$currentrec->{g1_250}, -onvalue=>"1", -offvalue=>"")->pack(-side=>'left');
my $g2250CB=$g250F->Checkbutton(-takefocus=>0, -state=>'disabled', -variable=>\$currentrec->{g2_250}, -onvalue=>"1", -offvalue=>"")->pack(-side=>'left');
my $g3250CB=$g250F->Checkbutton(-takefocus=>0, -state=>'disabled', -variable=>\$currentrec->{g3_250}, -onvalue=>"1", -offvalue=>"")->pack(-side=>'left');
my $g250L=$g250F->Label(-anchor=>'w', -font=>'arial 12 normal', -background=>'light grey', -text=>"250 Game")->pack(-side=>'left',-fill=>'x', -expand=>1);
my $g275F=$gcol->Frame(-background=>'light grey')->pack(-side=>'top',-fill=>'x');
my $g1275CB=$g275F->Checkbutton(-takefocus=>0, -state=>'disabled', -variable=>\$currentrec->{g1_275}, -onvalue=>"1", -offvalue=>"")->pack(-side=>'left');
my $g2275CB=$g275F->Checkbutton(-takefocus=>0, -state=>'disabled', -variable=>\$currentrec->{g2_275}, -onvalue=>"1", -offvalue=>"")->pack(-side=>'left');
my $g3275CB=$g275F->Checkbutton(-takefocus=>0, -state=>'disabled', -variable=>\$currentrec->{g3_275}, -onvalue=>"1", -offvalue=>"")->pack(-side=>'left');
my $g275L=$g275F->Label(-anchor=>'w', -font=>'arial 12 normal', -background=>'light grey', -text=>"275 Game")->pack(-side=>'left',-fill=>'x', -expand=>1);

my $scol=$statsF->Frame(-background=>'light grey')->pack(-side=>'left',-fill=>'both', -expand=>1);
my $s600F=$scol->Frame(-background=>'light grey')->pack(-side=>'top',-fill=>'x');
my $s600CB=$s600F->Checkbutton(-takefocus=>0, -state=>'disabled', -variable=>\$currentrec->{s600}, -onvalue=>"1", -offvalue=>"")->pack(-side=>'left');
my $s600L =$s600F->Label(-anchor=>'w', -font=>'arial 12 normal', -background=>'light grey', -text=>"600 Series")->pack(-side=>'left',-fill=>'x', -expand=>1);
my $s650F=$scol->Frame(-background=>'light grey')->pack(-side=>'top',-fill=>'x');
my $s650CB=$s650F->Checkbutton(-takefocus=>0, -state=>'disabled', -variable=>\$currentrec->{s650}, -onvalue=>"1", -offvalue=>"")->pack(-side=>'left');
my $s650L =$s650F->Label(-anchor=>'w', -font=>'arial 12 normal', -background=>'light grey', -text=>"650 Series")->pack(-side=>'left',-fill=>'x', -expand=>1);
my $s700F=$scol->Frame(-background=>'light grey')->pack(-side=>'top',-fill=>'x');
my $s700CB=$s700F->Checkbutton(-takefocus=>0, -state=>'disabled', -variable=>\$currentrec->{s700}, -onvalue=>"1", -offvalue=>"")->pack(-side=>'left');
my $s700L =$s700F->Label(-anchor=>'w', -font=>'arial 12 normal', -background=>'light grey', -text=>"700 Series")->pack(-side=>'left',-fill=>'x', -expand=>1);

my $tcol=$statsF->Frame(-background=>'light grey')->pack(-side=>'left',-fill=>'both', -expand=>1);
my $regF=$tcol->Frame(-background=>'light grey')->pack(-side=>'top',-fill=>'x');
my $regCB=$regF->Checkbutton(-takefocus=>0, -state=>'disabled', -variable=>\$currentrec->{reg}, -onvalue=>"1", -offvalue=>"")->pack(-side=>'left');
my $regL =$regF->Label(-anchor=>'w', -font=>'arial 12 normal', -background=>'light grey', -text=>"Regular")->pack(-side=>'left',-fill=>'x', -expand=>1);
my $srmF=$tcol->Frame(-background=>'light grey')->pack(-side=>'top',-fill=>'x');
my $srmCB=$srmF->Checkbutton(-takefocus=>0, -state=>'disabled', -variable=>\$currentrec->{srm}, -onvalue=>"1", -offvalue=>"")->pack(-side=>'left');
my $srmL =$srmF->Label(-anchor=>'w', -font=>'arial 12 normal', -background=>'light grey', -text=>"Women/Sr Men")->pack(-side=>'left',-fill=>'x', -expand=>1);
my $srwF=$tcol->Frame(-background=>'light grey')->pack(-side=>'top',-fill=>'x');
my $srwCB=$srwF->Checkbutton(-takefocus=>0, -state=>'disabled', -variable=>\$currentrec->{srw}, -onvalue=>"1", -offvalue=>"")->pack(-side=>'left');
my $srwL =$srwF->Label(-anchor=>'w', -font=>'arial 12 normal', -background=>'light grey', -text=>"Sr Women")->pack(-side=>'left',-fill=>'x', -expand=>1);


#$game3E->bind('<FocusOut>',sub { &recalc_stats($currentrec); $commitB->focusForce(); $commitB->focus(); });

$resetB=$formbuttonsF->Button(-font=>'times 12 normal', -takefocus=>0, -text=>'New/Clear Record', -command=>sub { 
	&clearrec($currentrec); 
	$leaguenameME->focus(); 
	$recpoint=scalar(@recsort);
}, -state=>'normal')->pack(-side=>'right');
my $recalcB=$formbuttonsF->Button(-font=>'times 12 normal', -takefocus=>0, -text=>'Recalc Fields', -command=>sub { &recalc_all($currentrec); }, -state=>'normal')->pack(-side=>'right');

$leaguenameME->focus();

MainLoop;

