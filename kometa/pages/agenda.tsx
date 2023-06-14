import { LoginForm } from '@/components/LoginForm';
import { useAuth } from '@/lib/use-auth';

function Agenda() {
  const { user } = useAuth();

  if (!user) {
      return <LoginForm />;
  }

  return <>Agenda</>;
}

export default Agenda;
