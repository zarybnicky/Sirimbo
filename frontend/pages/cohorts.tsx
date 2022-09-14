import { useAuth } from 'lib/data/use-auth';
import * as React from 'react';

export const GroupsPage = ({ }) => {
  // require auth
  const { user } = useAuth();
  return null;
}

export default GroupsPage;
