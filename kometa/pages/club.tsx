import { LoginForm } from '@/components/LoginForm';
import { useAuth } from '@/lib/use-auth';

function Klub() {
  const { user } = useAuth();

  if (!user) {
      return <LoginForm />;
  }

  return <>Klub</>;
}

export default Klub;
