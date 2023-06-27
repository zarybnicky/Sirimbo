import { LoginForm } from 'components/LoginForm';
import { useAuth } from '@app/ui/use-auth';
import { useQuery } from 'urql';
import { CurrentTenantDocument } from '@app/graphql/Tenant';

function Klub() {
  const { user } = useAuth();
  const [{ data }] = useQuery({query: CurrentTenantDocument, pause: !user });

  if (!user) {
      return <LoginForm />;
  }

  return <pre>{JSON.stringify(data, null, 2 )}</pre>;
}

export default Klub;
