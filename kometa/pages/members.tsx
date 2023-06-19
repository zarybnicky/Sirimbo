import { LoginForm } from 'components/LoginForm';
import { useAuth } from 'lib/use-auth';
import { CohortListWithMembersDocument } from '@app/graphql/Cohorts';
import { useQuery} from 'urql';

function Members() {
  const { user } = useAuth();
  const [{ data: cohorts }] = useQuery({
    query: CohortListWithMembersDocument,
    variables: { visible: true },
  });

  if (!user) {
      return <LoginForm />;
  }

  return <pre>{JSON.stringify(cohorts, null, 2)}</pre>
}

export default Members;
