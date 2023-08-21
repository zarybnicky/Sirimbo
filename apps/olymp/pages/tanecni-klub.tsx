import { CurrentTenantDocument } from '@app/graphql/Tenant';
import { RichTextView } from '@app/ui/RichTextView';
import { TenantForm } from '@app/ui/TenantForm';
import { TitleBar } from '@app/ui/TitleBar';
import { Dialog, DialogContent, DialogTrigger } from '@app/ui/dialog';
import { buttonCls } from '@app/ui/style/button';
import { useAuth } from '@app/ui/use-auth';
import { Layout } from 'components/layout/Layout';
import { Edit } from 'lucide-react';
import React from 'react';
import { useQuery } from 'urql';

const Page = () => {
  const [open, setOpen] = React.useState(false);
  const { perms } = useAuth();
  const [{ data }] = useQuery({query: CurrentTenantDocument});

  const tenant = data?.tenant;
  if (!tenant) return null;

  return (
    <Layout requireMember>
      <TitleBar title={tenant.name}>
        {perms.isAdmin && (
          <Dialog open={open} onOpenChange={setOpen}>
            <DialogTrigger asChild>
              <button className={buttonCls()}>
                <Edit />
                Upravit
              </button>
            </DialogTrigger>
            <DialogContent>
              <TenantForm onSuccess={() => setOpen(false)} />
            </DialogContent>
          </Dialog>
        )}
      </TitleBar>

      <RichTextView value={tenant.memberInfo} />

      <div className="my-2">
        Trenéři
        {tenant.tenantTrainersList.map(x => (
          <div key={x.id}>
            {x.person?.firstName} {x.person?.lastName}
          </div>
        ))}
      </div>

      <div className="my-2">
        Správci
        {tenant.tenantAdministratorsList.map(x => (
          <div key={x.id}>
            {x.person?.firstName} {x.person?.lastName}
          </div>
        ))}
      </div>

      <div className="my-2">
        Lokality/Sály
        {tenant.tenantLocationsList.map(x => (
          <div key={x.id}>
            {x.location?.name}
          </div>
        ))}
      </div>

    </Layout>
  );
};

export default Page;
