import React from 'react';
import { PersonDocument } from '@app/graphql/Person';
import { TitleBar } from '@app/ui/TitleBar';
import { useQuery } from 'urql';
import { formatFullName, formatLongCoupleName } from '@app/ui/format';
import { Dialog, DialogContent, DialogTrigger } from '@app/ui/dialog';
import { useAuth } from '@app/ui/use-auth';
import { buttonCls } from '@app/ui/style';
import { Edit } from 'lucide-react';
import { PersonForm } from '@app/ui/PersonForm';
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
  const [open, setOpen] = React.useState(false);
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
        {perms.isAdmin && (
          <Dialog open={open} onOpenChange={setOpen}>
            <DialogTrigger asChild>
              <button className={buttonCls({ variant: 'outline' })}>
                <Edit />
                Upravit
              </button>
            </DialogTrigger>
            <DialogContent>
              <PersonForm id={id} onSuccess={() => setOpen(false)} />
            </DialogContent>
          </Dialog>
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
