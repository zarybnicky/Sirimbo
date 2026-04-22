import { Layout } from '@/ui/Layout';
import { MyMembershipApplicationsDocument } from '@/graphql/CurrentUser';
import { RichTextView } from '@/ui/RichTextView';
import { PageHeader, TitleBar } from '@/ui/TitleBar';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { formatOpenDateRange, moneyFormatter } from '@/ui/format';
import { CreateMembershipApplicationForm } from '@/ui/forms/CreateMembershipApplicationForm';
import { EditTenantLocationForm } from '@/ui/forms/EditLocationForm';
import { EditTenantForm } from '@/ui/forms/EditTenantForm';
import { typographyCls } from '@/ui/style';
import { useAuth } from '@/ui/use-auth';
import { Pencil } from 'lucide-react';
import Link from 'next/link';
import { useQuery } from 'urql';
import { CurrentTenantDocument } from '@/graphql/Tenant';
import { useActionMap, useActions } from '@/lib/actions';
import { tenantAdministratorActions } from '@/lib/actions/tenantAdministrator';
import { tenantLocationActions } from '@/lib/actions/tenantLocation';
import { tenantTrainerActions } from '@/lib/actions/tenantTrainer';
import { ActionRow } from '@/ui/ActionRow';

export default function ClubPage() {
  const auth = useAuth();
  const [{ data: tenant }] = useQuery({ query: CurrentTenantDocument });
  const [{ data: applications }] = useQuery({ query: MyMembershipApplicationsDocument });
  const administratorActionMap = useActionMap(
    tenantAdministratorActions,
    tenant?.tenant?.tenantAdministratorsList ?? [],
  );
  const trainerActionMap = useActionMap(
    tenantTrainerActions,
    tenant?.tenant?.tenantTrainersList ?? [],
  );
  const locationActionMap = useActionMap(
    tenantLocationActions,
    tenant?.tenant?.tenantLocationsList ?? [],
  );
  const tenantActions = useActions(
    [
      {
        id: 'tenant.edit',
        group: 'primary',
        label: 'Upravit klub',
        icon: Pencil,
        visible: ({ auth }) => auth.isAdmin,
        render: () => <EditTenantForm />,
      },
    ],
    tenant?.tenant,
  );

  if (!tenant?.tenant) return null;

  return (
    <Layout requireMember>
      <PageHeader title="Klub" actions={tenantActions} />

      <RichTextView value={tenant.tenant.description} />

      <h2 className={typographyCls({ variant: 'section', className: 'my-3' })}>
        Trenéři
      </h2>
      {tenant.tenant.tenantTrainersList.map((item) => (
        <ActionRow key={item.id} actions={trainerActionMap.get(item.id)!}>
          <div className="grow gap-3 align-baseline flex flex-wrap justify-between text-sm py-1">
            {!item.person ? (
              '?'
            ) : (
              <Link
                className="underline font-bold grow basis-40"
                href={{
                  pathname: '/clenove/[id]',
                  query: { id: item.person?.id },
                }}
              >
                {item.person?.name}
              </Link>
            )}
            {auth.isAdmin && (
              <>
                <div className="self-end">
                  {moneyFormatter.format({
                    amount: item.memberPrice45MinAmount,
                    currency: item.currency,
                  }) || '-'}{' '}
                  {item.guestPrice45MinAmount &&
                  item.memberPrice45MinAmount !== item.guestPrice45MinAmount
                    ? `(${moneyFormatter.format({ amount: item.guestPrice45MinAmount, currency: item.currency })})`
                    : ''}
                  {' / 45min'}
                </div>
                <span className="grow basis-40 text-right">
                  {formatOpenDateRange(item)}
                </span>
              </>
            )}
          </div>
        </ActionRow>
      ))}

      {auth.isAdmin && (
        <>
          <h2 className={typographyCls({ variant: 'section', className: 'my-3' })}>
            Správci
          </h2>
          {tenant.tenant.tenantAdministratorsList.map((data) => (
            <ActionRow key={data.id} actions={administratorActionMap.get(data.id)!}>
              {!data.person ? (
                '?'
              ) : (
                <Link
                  className="underline font-bold text-sm py-1"
                  href={{
                    pathname: '/clenove/[id]',
                    query: { id: data.person?.id },
                  }}
                >
                  {data.person?.name}
                </Link>
              )}
            </ActionRow>
          ))}

          <TitleBar title="Lokality/sály" variant="section" className="mt-3">
            <Dialog>
              <DialogTrigger.Add size="sm" />
              <DialogContent>
                <EditTenantLocationForm />
              </DialogContent>
            </Dialog>
          </TitleBar>

          {tenant.tenant.tenantLocationsList.map((item) => (
            <ActionRow key={item.id} actions={locationActionMap.get(item.id)!}>
              <div className="grow gap-2 flex text-sm font-bold py-1">{item.name}</div>
            </ActionRow>
          ))}

          {!!applications?.membershipApplicationsList?.length && (
            <>
              <h2 className={typographyCls({ variant: 'section', className: 'my-3' })}>
                Žádosti o členství
              </h2>

              {applications.membershipApplicationsList.map((x) => (
                <Dialog key={x.id}>
                  <DialogTrigger.Edit text={`${x.firstName} ${x.lastName}`} />
                  <DialogContent>
                    <CreateMembershipApplicationForm data={x} />
                  </DialogContent>
                </Dialog>
              ))}
            </>
          )}
        </>
      )}
    </Layout>
  );
}
