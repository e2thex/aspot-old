#!/usr/bin/perl 

use Data::Dumper;
use ASPOTSubject;
package ASPOTStore;

sub new {
  my $class = shift;
  my $self = { 
    _host => shift,
    _store => shift,
    _user => shift,
    _pass => shift,
    _statements => [],
    _subjects =>{},
  };
  bless $self, $class;
  return $self;
}
sub deleteObj {
  my ($self, $uuid) = @_;
  print Dumper({ action=>'delete_obj', uuid=>$uuid});
}
sub deleteStatement {
  my ($self, $uuid) = @_;
  print Dumper({ action=>'delete_statement', uuid=>$uuid});
}
sub statement {
  my ($self, $statement) = @_;
    push @{$self->{_statements}}, $statement;

}
sub updateStatement {
  my ($self, $subject, $predict, $object) = @_;
  my $uuid = $self->uuid;
    push @{$self->{_statements}},
      {
        subject => $subject,
        predict => $predict,
        object  => $object,
      }
    ;
  return $uuid;
  
}
sub find {
  my ($self, $query, $uuid) = @_;
  print Dumper({ action=>'find', query=>$uuid."/".$query});
}

sub uuid {
  my $uuid =  `/usr/bin/uuidgen`;
  $uuid=~s/\n//;
  return $uuid;
}
sub newSubject {
  my ($self,$uuid) = @_;
  $uuid = ASPOTStore->uuid() if !$uuid;
  my $subject = new ASPOTSubject($self, $uuid,[]);
  $self->{_subjects}->{$uuid} = $subject;
  return $subject;
}
sub getSubject {
  my ($self,$uuid) = @_;
  my $subject;
  if ($subject = $self->{_subjects}->{$uuid}) {
  }
  #elsif (0) {
  #  # query Store for object
  #}
  else {
    $subject = $self->newSubject($uuid);
    
  }
  #print Dumper($subject);
  return $subject;
  
}
sub save {
  my ($self,$transactions) = @_;
  #print Dumper($transactions);

}
1;
