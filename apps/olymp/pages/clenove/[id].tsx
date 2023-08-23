import React from 'react';
import { PersonDocument } from '@app/graphql/Person';
import { TitleBar } from '@app/ui/TitleBar';
import { useQuery } from 'urql';
import { formatFullName, formatLongCoupleName } from '@app/ui/format';
import { useAuth } from '@app/ui/use-auth';
import { EditPersonDialog } from '@app/ui/EditPersonDialog';
import { useRouter } from 'next/router';
import { fromSlugArray } from '@app/ui/slugify';
import { Layout } from 'components/layout/Layout';
import { formatOpenDateRange } from '@app/ui/format';
import Link from 'next/link';
import { PersonList } from '@app/ui/PersonList';
import { WithSidebar } from '@app/ui/WithSidebar';

function PersonPage() {
  const router = useRouter();
  const id = fromSlugArray(router.query.id);
  const { perms } = useAuth();
  const [{ data }] = useQuery({ query: PersonDocument, variables: { id }, pause: !id });

  const item = data?.person;
  if (!item) {
    return (
      <Layout requireMember>
        <WithSidebar sidebar={<PersonList />}/>
      </Layout>
    );
  };

  return (
    <Layout requireMember>
      <WithSidebar sidebar={<PersonList />}>
      <TitleBar title={formatFullName(item)}>
        {(perms.isAdmin || perms.isCurrentPerson(item.id)) && (
          <EditPersonDialog id={id} />
        )}
      </TitleBar>

      <div className="prose mb-2">
        <h3>Členství</h3>
        {item.tenantMembershipsList?.map((item) => (
          <div key={item.id}>
            Člen klubu {formatOpenDateRange(item)}
          </div>
        ))}
        {item.tenantAdministratorsList?.map((item) => (
          <div key={item.id}>
            Správce klubu {formatOpenDateRange(item)}
          </div>
        ))}
        {item.tenantTrainersList?.map((item) => (
          <div key={item.id}>
            Trenér {formatOpenDateRange(item)}
          </div>
        ))}

        {!!item.couplesList?.length && <h3>Páry</h3>}
        {item.couplesList?.map((item) => (
          <div key={item.id}>
            <Link href={`/pary/${item.id}`}>
              {formatLongCoupleName(item)}
            </Link>
            {' '}
            ({formatOpenDateRange(item)})
          </div>
        ))}

        {!!item.cohortMembershipsList?.length && <h3>Tréninkové skupiny</h3>}
        {item.cohortMembershipsList?.map((item) => (
          <div key={item.id}>
            <Link href={`/treninkove-skupiny/${item.cohort?.id}`}>
              {item.cohort?.sName}
            </Link>
            {' '}
            ({formatOpenDateRange(item)})
          </div>
        ))}
      </div>
      </WithSidebar>
    </Layout>
  );
}

export default PersonPage;
