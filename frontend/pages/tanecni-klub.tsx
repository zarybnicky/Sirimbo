import { Layout } from '@/components/layout/Layout';
import { MyMembershipApplicationsDocument } from '@/graphql/CurrentUser';
import { RichTextView } from '@/ui/RichTextView';
import { TitleBar } from '@/ui/TitleBar';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { DropdownMenu, DropdownMenuContent, DropdownMenuTrigger } from '@/ui/dropdown';
import { CreateMembershipApplicationForm } from '@/ui/forms/CreateMembershipApplicationForm';
import { EditTenantLocationForm } from '@/ui/forms/EditLocationForm';
import { EditTenantForm } from '@/ui/forms/EditTenantForm';
import { TenantAdministratorMenu } from '@/ui/menus/TenantAdministratorMenu';
import { TenantLocationMenu } from '@/ui/menus/TenantLocationMenu';
import { TenantTrainerMenu } from '@/ui/menus/TenantTrainerMenu';
import { typographyCls } from '@/ui/style';
import { useAuth } from '@/ui/use-auth';
import { useTenant } from '@/ui/useTenant';
import Link from 'next/link';
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
        <div className="flex gap-3 mb-1 align-baseline" key={data.id}>
          {auth.isAdmin && (
            <TenantTrainerMenu align="start" data={data}>
              <DropdownMenuTrigger.RowDots />
            </TenantTrainerMenu>
          )}
          <div className="grow gap-2 align-baseline flex flex-wrap justify-between text-sm py-1">
            <Link className="underline font-bold" href={`/clenove/${data.person?.id}`}>{data.person?.name}</Link>
            {auth.isAdmin && (
              <div className="flex flex-wrap gap-4">
                <span>
                  {data.memberPrice45Min?.amount ?? '- '}
                  {'Kč '}
                  {data.guestPrice45Min ? ('(' + data.guestPrice45Min.amount + 'Kč)') : ''}
                  {' / 45min'}
                </span>
              </div>
            )}
          </div>
        </div>
      ))}

      <h2 className={typographyCls({ variant: 'section', className: 'my-3' })}>
        Správci
      </h2>
      {tenant.tenantAdministratorsList.map((data) => (
        <div className="flex gap-3 mb-1" key={data.id}>
          {auth.isAdmin && (
            <TenantAdministratorMenu align="start" data={data}>
              <DropdownMenuTrigger.RowDots />
            </TenantAdministratorMenu>
          )}
          <div className="grow gap-2 align-baseline flex flex-wrap justify-between text-sm py-1">
            <Link className="underline font-bold" href={`/clenove/${data.person?.id}`}>
              {data.person?.name}
            </Link>
          </div>
        </div>
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
      {tenant.tenantLocationsList.map((item) => (
        <div className="flex gap-3 mb-1" key={item.id}>
          {auth.isAdmin && (
            <TenantLocationMenu id={item.id} align="start">
              <DropdownMenuTrigger.RowDots />
            </TenantLocationMenu>
          )}
          <div className="grow gap-2 flex text-sm py-1">
            <b>{item.name}</b>
          </div>
        </div>
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
