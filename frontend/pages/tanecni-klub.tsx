import { CurrentTenantDocument } from '@app/graphql/Tenant';
import { RichTextView } from '@app/ui/RichTextView';
import { EditTenantDialog } from '@app/ui/EditTenantDialog';
import { TitleBar } from '@app/ui/TitleBar';
import { typographyCls } from '@app/ui/style';
import { useAuth } from '@app/ui/use-auth';
import { Layout } from '@/components/layout/Layout';
import React from 'react';
import { useQuery } from 'urql';
import { EditTenantAdministratorCard } from '@app/ui/EditTenantAdministratorForm'
import { EditTenantTrainerCard } from '@app/ui/EditTenantTrainerForm'
import { EditTenantLocationCard } from '@/ui/EditLocationForm';

const Page = () => {
  const { perms } = useAuth();
  const [{ data }] = useQuery({ query: CurrentTenantDocument });
  const tenant = data?.tenant;
  if (!tenant) return null;

  return (
    <Layout requireMember>
      <TitleBar title="Klub">{perms.isAdmin && <EditTenantDialog />}</TitleBar>

      <RichTextView value={tenant.description} />

      <h2 className={typographyCls({ variant: 'section', className: 'my-3' })}>
        Trenéři
      </h2>
      {tenant.tenantTrainersList.filter(x => x.active).map((data) => (
        <EditTenantTrainerCard key={data.id} data={data} showPerson />
      ))}

      <h2 className={typographyCls({ variant: 'section', className: 'my-3' })}>
        Správci
      </h2>
      {tenant.tenantAdministratorsList.map((data) => (
        <EditTenantAdministratorCard key={data.id} data={data} showPerson />
      ))}

      <h2 className={typographyCls({ variant: 'section', className: 'my-3' })}>
        Lokality/sály
      </h2>
      {tenant.tenantLocationsList.map((x) => (
        <EditTenantLocationCard key={x.id} data={x} />
      ))}
    </Layout>
  );
};

export default Page;
