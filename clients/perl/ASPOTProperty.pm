#!/usr/bin/perl 
package ASPOTProperty;
use Data::Dumper;
use Scalar::Util;
use ASPOTStore;
use ASPOTSubject;
use ASPOTValue;

sub new {
  my ($class, $subject, $predicate) = @_;
  my $self = { 
    _subject => $subject,
    _predicate => $predicate,
  };
  bless $self, $class;
  return $self
}
sub value {
  my ($self, $object) = @_;
  my $values = $self->_values;
  if ($values->[0]) {
    return $values->[0]->value($object);
  }
  else {
  return $self->add($object);
  }
 
}

sub _values {
	my ($self) = @_;
    my $subject = $self->{_subject};
    my $predicate = $self->{_predicate};
    my $uuids = $subject->statement_by_predicate($predicate);
    my @values = map { new ASPOTValue($self,$_) } @$uuids;
    return \@values;
}
sub _value {
  my ($self,$statement_uuid, $object) = @_;
  $subject = $self->{_subject};
  $predicate = $self->{_predicate};
  return $subject->_value($statement_uuid,$predicate, $object);
}
sub _remove_value {
  my ($self,$statement_uuid) = @_;
  return $subject->_value($statement_uuid,undef, undef);
  
}

sub _get_object {
  my ($self,$statement_uuid, $name) = @_;
  return $self->{_subject}->_get_object($statement_uuid);

}

sub add { 
	my ($self, $object) = @_;
    my $uuid = ASPOTStore->uuid();
    my $new = new ASPOTValue($self,$uuid);
    if ($object) {
      $new->value($object);
    }
    return $new; 
}
sub foreach {
	my ($self, $subref) = @_;
	foreach my $val (@{$self->_values}) {
          $subref->($val); # or $self->$subref($val)
        }
}


sub DESTROY { }
sub AUTOLOAD {
  my($self) = @_;
  my $name = $AUTOLOAD;
  $name =~ s/.*://;   # strip fully-qualified portion
  my ($self) = @_;
  my $values = $self->_values;
  if ($values->[0]) {
    #print Dumper($values);<STDIN>;
    return $values->[0]->$name;
  }
  else {
  my $uuid = ASPOTStore->uuid();
  my $subject = ASPOTStore->uuid();
  my $new = $self->_value($uuid,$subject);
  return $self->$name;
  }
}

1;
