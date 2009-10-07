#!/usr/bin/perl 

use Scalar::Util;
package RDFValue;
our $AUTOLOAD;

sub new {
  my ($class, $property, $statement_uuid) = @_;

  my $self = {
    _property => $property,
    _statement_uuid => $statement_uuid,
  };
  bless $self,$class;
  return $self;
}
sub value {
  my($self, $value) = @_;
  #if view value is a RDFSubject get the uuid as the value
  if (ref $value eq RDFSubject) {
    $value = $value->value;
  }
  return $self->{_property}->_value($self->{_statement_uuid},$value);
}
sub remove {
  my($self) = @_;
  return $self->{_property}->_remove_value($self->{_statement_uuid});


}

sub AUTOLOAD {
  my($self) = @_;
  my $name = $AUTOLOAD;
  $name =~ s/.*://;   # strip fully-qualified portion
  print Dumper($self->{_property});<STDIN>;
  #my $subject = $self->{_property}->_get_object($self->{_statement_uuid});
  print Dumper($subject);<STDIN>;
  return $subject->$name;
}
1;
