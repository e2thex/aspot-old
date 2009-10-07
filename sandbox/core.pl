#!/usr/bin/perl 

use Data::Dumper;
use lib "./";
use RDFStore;


my $store = new RDFStore('a','b','c','d');
my $obj = $store->newSubject();
my $add = $store->newSubject();
$add->line1->value("home");
$obj->address->value($add);
$obj->address->line2->value("baseball");
print Dumper($store);
#$obj->phone->add("joe");
#print Dumper( $obj->subject);

#print Dumper($obj->statement_by_predicate('phone'));
#print $obj->phone->value();
#print Dumper($obj->phone->_values());
#print Dumper($store);
#print Dumper($store);
#print Dumper($phone);

exit;
