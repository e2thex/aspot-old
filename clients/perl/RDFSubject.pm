#!/usr/bin/perl 

use Scalar::Util;
use RDFStore;
use RDFProperty;
package RDFSubject;
our $AUTOLOAD;

sub new
{
    my ($class,$store, $subject,$statements) = @_;
    #if (reftype($store)->class ne 'RDFStore') {
    #}
    $subject = $store->uuid if !$subject;
    my $properties = map { 
      $_->{uuid} => { $_->{predicate} => $_->{object} }
    } @$statements;
    my $self = {
        _store => $store,
        _subject => $subject,
        _statements => {},
        _transactions => [],
    };
    bless $self, $class;
    return $self;
}

# get the statemts object and then ashe the store for a subject base on it
sub _get_object {
  my ($self,$statement_uuid) = @_;
    my $uuid = $self->{_statements}->{$statement_uuid}->{object};
    my $subject = $self->{_store}->getSubject($uuid);
    return $subject;

}
# giben a predicate return an arrayref of all the statement uuid ussing that predicate
sub statement_by_predicate {
  my ($self,$predicate) = @_;
  my @v = grep {$self->{_statements}->{$_}->{predicate} eq $predicate} keys %{$self->{_statements}};
  my @v = sort @v;
  return \@v;
}
# an accesor, given a statement uuid return the value of the object, or if obkect is stateed set it.
sub _value {
  my ($self,$statement_uuid, $predicate, $object) = @_;
  if ($object) {
    $self->{_statements}->{$statement_uuid} = {
      predicate => $predicate,
      object   => $object,
    };
    my $statement = {
      uuid      => $statement_uuid,
      subject   => $self->{_subject},
      predicate => $predicate,
      object   => $object,
    };
    push @{$self->{_transactions}}, $statement;


  }
  if (defined $predicate) {
    
    my $uuid = $self->{_statements}->{$statement_uuid}->{object};
    return $self->{_store}->getSubject($uuid);

  }
  else {
    $self->{_statements}->{$statement_uuid} = undef;
    my $statement = {
      uuid      => $statement_uuid,
      subject   => undef,
      predicate => undef,
      subject   => undef,
    };
    push @{$self->{_transactions}}, $statement;
    
  }
}

sub save {
  my ( $self ) = @_;
  $self->{_store}->save($self->{_transactions});
  $self->{_transactions} = [];
}


# resturn the value os the subject
sub value {
    my ( $self ) = @_;
    return $self->{_subject};
}
#sub delete {
#    my ( $self ) = @_;
#    return $self->{_store}->deleteObj($self->{_uuid});
#}
#sub find {
#    my ( $self, $query ) = @_;
#    return $self->{_store}->find($self->{_uuid}, $query);
#}
#sub properties {
#    my ( $self ) = @_;
#    return keys %{ $self->{_properties} }
#}

sub AUTOLOAD {
    my($self) = @_;
	my $name = $AUTOLOAD;
	$name =~ s/.*://;   # strip fully-qualified portion
   # dpm($self->{_store});<STDIN>;
    my $property = new RDFProperty($self,$name);
    return $property;
}    
1;

