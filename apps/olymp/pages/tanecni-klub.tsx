import { CurrentTenantDocument } from '@app/graphql/Tenant';
import { RichTextView } from '@app/ui/RichTextView';
import { EditTenantDialog } from '@app/ui/EditTenantDialog';
import { TitleBar } from '@app/ui/TitleBar';
import { typographyCls } from '@app/ui/style';
import { useAuth } from '@app/ui/use-auth';
import { Layout } from 'components/layout/Layout';
import Link from 'next/link';
import React from 'react';
import { useQuery } from 'urql';

const Page = () => {
  const { perms } = useAuth();
  const [{ data }] = useQuery({query: CurrentTenantDocument});
  const tenant = data?.tenant;
  if (!tenant) return null;

  return (
    <Layout requireMember>
      <TitleBar title="Klub">
        {perms.isAdmin && (
          <EditTenantDialog />
        )}
      </TitleBar>

      <RichTextView value={tenant.memberInfo} />

      <h2 className={typographyCls({ variant: 'section', className: "pt-4" })}>Trenéři</h2>
      <ul className="list-style-none">
        {tenant.tenantTrainersList.map(x => (
          <li key={x.id}>
            <Link href={`/clenove/${x.person!.id}`} className="text-primary-11 underline">
              {x.person?.firstName} {x.person?.lastName}
            </Link>
          </li>
        ))}
      </ul>

      <h2 className={typographyCls({ variant: 'section', className: "pt-4" })}>Správci</h2>
      {tenant.tenantAdministratorsList.map(x => (
        <div key={x.id}>
          {x.person?.firstName} {x.person?.lastName}
        </div>
      ))}

      <h2 className={typographyCls({ variant: 'section', className: "pt-4" })}>Lokality/sály</h2>
      {tenant.tenantLocationsList.map(x => (
        <div key={x.id}>
          {x.location?.name}
        </div>
      ))}
    </Layout>
  );
};

export default Page;
