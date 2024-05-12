import { RichTextView } from '@/ui/RichTextView';
import { EditTenantDialog } from '@/ui/EditTenantDialog';
import { TitleBar } from '@/ui/TitleBar';
import { buttonCls, typographyCls } from '@/ui/style';
import { useAuth } from '@/ui/use-auth';
import { Layout } from '@/components/layout/Layout';
import React from 'react';
import { useQuery } from 'urql';
import { EditTenantLocationForm } from '@/ui/forms/EditLocationForm';
import { MyMembershipApplicationsDocument } from '@/graphql/CurrentUser';
import { MembershipApplicationCard } from '@/ui/forms/CreateMembershipApplicationForm';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { Plus } from 'lucide-react';
import { useTenant } from '@/ui/useTenant';
import {EditTenantLocationCard} from "@/ui/EditTenantLocationCard";
import {EditTenantTrainerCard} from "@/ui/EditTenantTrainerCard";
import {EditTenantAdministratorCard} from "@/ui/EditTenantAdministratorCard";

const Page = () => {
  const auth = useAuth();
  const { data: tenant } = useTenant();
  const [{ data: applications }] = useQuery({ query: MyMembershipApplicationsDocument });
  const [addOpen, setAddOpen] = React.useState(false);

  if (!tenant) return null;

  return (
    <Layout requireMember>
      <TitleBar title="Klub">
        {auth.isAdmin && <EditTenantDialog />}
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
        {auth.isAdmin && (
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

      {(auth.isAdmin && !!applications?.membershipApplicationsList?.length) && (
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
