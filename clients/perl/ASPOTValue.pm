
#!/usr/bin/perl 

use Data::Dumper;
use Scalar::Util;
use ASPOTStore;
use ASPOTProperty;
use ASPOTSubject;


package ASPOTValue;
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
  #if view value is a ASPOTRDFSubject get the uuid as the value
  if (ref $value eq ASPOTSubject) {
    $value = $value->value;
  }
  return $self->{_property}->_value($self->{_statement_uuid},$value);
}
sub remove {
  my($self) = @_;
  return $self->{_property}->_remove_value($self->{_statement_uuid});


}
sub DESTROY { }
sub AUTOLOAD {
  my($self) = @_;
  my $name = $AUTOLOAD;
  $name =~ s/.*://;   # strip fully-qualified portion
  my $subject = $self->{_property}->_get_object($self->{_statement_uuid});
  return $subject->$name;
}



1;

