import { CurrentTenantDocument } from '@app/graphql/Tenant';
import { RichTextView } from '@app/ui/RichTextView';
import { EditTenantDialog } from '@app/ui/EditTenantDialog';
import { TitleBar } from '@app/ui/TitleBar';
import { buttonCls, typographyCls } from '@app/ui/style';
import { useAuth } from '@app/ui/use-auth';
import { Layout } from '@/components/layout/Layout';
import React from 'react';
import { useQuery } from 'urql';
import { EditTenantAdministratorCard } from '@app/ui/EditTenantAdministratorForm'
import { EditTenantTrainerCard } from '@app/ui/EditTenantTrainerForm'
import { EditTenantLocationCard, EditTenantLocationForm } from '@/ui/EditLocationForm';
import { MyMembershipApplicationsDocument } from '@/graphql/CurrentUser';
import { MembershipApplicationCard } from '@/ui/CreateMembershipApplicationForm';
import { Dialog, DialogContent, DialogTrigger } from '@app/ui/dialog';
import { Plus } from 'lucide-react';

const Page = () => {
  const { perms } = useAuth();
  const [{ data }] = useQuery({ query: CurrentTenantDocument });
  const [{ data: applications }] = useQuery({ query: MyMembershipApplicationsDocument });
  const tenant = data?.tenant;
  const [addOpen, setAddOpen] = React.useState(false);

  if (!tenant) return null;

  return (
    <Layout requireMember>
      <TitleBar title="Klub">
        {perms.isAdmin && <EditTenantDialog />}
      </TitleBar>

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

      <TitleBar title="Lokality/sály" variant='section' className="mt-3">
        {perms.isAdmin && (
          <Dialog open={addOpen} onOpenChange={setAddOpen}>
            <DialogTrigger asChild>
              <button className={buttonCls({ size: 'sm', variant: 'outline' })}>
                <Plus />
                Vytvořit
              </button>
            </DialogTrigger>
            <DialogContent>
              <EditTenantLocationForm onSuccess={() => setAddOpen(false)} />
            </DialogContent>
          </Dialog>
        )}
      </TitleBar>
      {tenant.tenantLocationsList.map((x) => (
        <EditTenantLocationCard key={x.id} data={x} />
      ))}

      {(perms.isAdmin && !!applications?.membershipApplicationsList?.length) && (
        <>
          <h2 className={typographyCls({ variant: 'section', className: 'my-3' })}>
            Žádosti o členství
          </h2>

          {applications.membershipApplicationsList.map(x => (
            <MembershipApplicationCard item={x} key={x.id} />
          ))}
        </>
      )}
    </Layout>
  );
};

export default Page;
