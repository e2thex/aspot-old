#!/usr/bin/perl 

use Scalar::Util;
package RDFStatementGroup;

sub new {
  my ($class, $store, $subject, $predicate, $sol) = @_;
  my $self = { 
    _store =>$store,
    _subject => $subject,
    _predicate => $predicate,
    _statment_object_list => $sol,
  };
  bless $self, $class;
  return $self
}
sub add {
  my ($self, $object) = @_;
  return $self->{_store}->deleteStatement($self->{uuid});
}
sub delete {
  my ($self, $value) = @_;
}
sub value {
  my ($self, $value) = @_;
  my $statement_uuid = (keys %hash)[-1];
  $statement_uuid = $self->{_store}->uuid if !statement_uuid;
  if ($value) {
    
    my $value = $value->can(uuid) ? $value->uuid : $value;
    
    my $statement = {
      uuid=>$statement_uuid,
      object=>$value,
    };
    $self->{_store}->statement($statement);
  }

 
}
sub AUTOLOAD {
	my $self = shift;
#	my $type = ref($self)
#		    or croak "$self is not an object";

	my $name = $AUTOLOAD;
	$name =~ s/.*://;   # strip fully-qualified portion
    k
    
}
1;
