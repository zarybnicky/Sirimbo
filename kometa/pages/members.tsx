import { LoginForm } from '@/components/LoginForm';
import { useAuth } from '@/lib/use-auth';

function Members() {
  const { user } = useAuth();

  if (!user) {
      return <LoginForm />;
  }

  return <>Members</>;
}

export default Members;
