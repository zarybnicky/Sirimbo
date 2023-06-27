import { LoginForm } from 'components/LoginForm';
import { useAuth } from '@app/ui/use-auth';
import React from 'react';
import { Calendar } from '@app/calendar/Calendar';

function DnDResource() {
  const { user } = useAuth();
  if (!user) {
    return <LoginForm />;
  }

  return <Calendar />;
}

export default DnDResource;
