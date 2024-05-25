import { Layout } from '@/components/layout/Layout';
import { MyMembershipApplicationsDocument } from '@/graphql/CurrentUser';
import { EditTenantAdministratorCard } from "@/ui/EditTenantAdministratorCard";
import { EditTenantLocationCard } from "@/ui/EditTenantLocationCard";
import { EditTenantTrainerCard } from "@/ui/EditTenantTrainerCard";
import { RichTextView } from '@/ui/RichTextView';
import { TitleBar } from '@/ui/TitleBar';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { CreateMembershipApplicationForm } from '@/ui/forms/CreateMembershipApplicationForm';
import { EditTenantLocationForm } from '@/ui/forms/EditLocationForm';
import { EditTenantForm } from '@/ui/forms/EditTenantForm';
import { typographyCls } from '@/ui/style';
import { useAuth } from '@/ui/use-auth';
import { useTenant } from '@/ui/useTenant';
import React from 'react';
import { useQuery } from 'urql';

const Page = () => {
  const auth = useAuth();
  const { data: tenant } = useTenant();
  const [{ data: applications }] = useQuery({ query: MyMembershipApplicationsDocument });

  if (!tenant) return null;

  return (
    <Layout requireMember>
      <TitleBar title="Klub">
        {auth.isAdmin && (
          <Dialog>
            <DialogTrigger.Edit />
            <DialogContent>
              <EditTenantForm />
            </DialogContent>
          </Dialog>
        )}
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
          <Dialog>
            <DialogTrigger.Add size="sm" />
            <DialogContent>
              <EditTenantLocationForm />
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
            <Dialog key={x.id}>
              <DialogTrigger.Edit text={`${x.firstName} ${x.lastName}`} />
              <DialogContent>
                <CreateMembershipApplicationForm data={x} />
              </DialogContent>
            </Dialog>
          ))}
        </>
      )}
    </Layout>
  );
};

export default Page;
