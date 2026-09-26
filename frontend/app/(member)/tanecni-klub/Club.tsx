'use client';

import { PendingMembershipApplicationsDocument } from '@/graphql/MembershipApplication';
import { RichTextView } from '@/ui/RichTextView';
import { PageHeader } from '@/ui/TitleBar';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { moneyFormatter } from '@/ui/format';
import { CreateMembershipApplicationForm } from '@/ui/forms/CreateMembershipApplicationForm';
import { EditTenantLocationForm } from '@/ui/forms/EditLocationForm';
import { EditTenantForm } from '@/ui/forms/EditTenantForm';
import { useAuth } from '@/lib/auth';
import { Pencil, PinIcon } from 'lucide-react';
import Link from 'next/link';
import { useQuery } from 'urql';
import { CurrentTenantDocument } from '@/graphql/Tenant';
import { useActionMap, useActions } from '@/lib/actions';
import { tenantAdministratorActions } from '@/lib/actions/tenantAdministrator';
import { tenantLocationActions } from '@/lib/actions/tenantLocation';
import { tenantTrainerActions } from '@/lib/actions/tenantTrainer';
import { ActionRow } from '@/ui/ActionRow';
import { CstsIdBackfillWidget } from '@/ui/CstsIdBackfillWidget';
import { TabMenu } from '@/ui/TabMenu';
import { parseAsString, useQueryState } from 'nuqs';

export function Club() {
  const auth = useAuth();
  const [tab, setTab] = useQueryState(
    'tab',
    parseAsString.withOptions({ history: 'push' }),
  );
  const [{ data: tenant }] = useQuery({ query: CurrentTenantDocument });
  const [{ data: applications }] = useQuery({
    query: PendingMembershipApplicationsDocument,
  });
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
        requireAdmin: true,
        render: () => <EditTenantForm />,
      },
      {
        id: 'tenant.addLocation',
        label: 'Přidat lokalitu',
        icon: PinIcon,
        requireAdmin: true,
        render: () => <EditTenantLocationForm />
      }
    ],
    tenant?.tenant,
  );

  if (!tenant?.tenant) return null;
  const club = tenant.tenant;
  const pendingApplications = applications?.membershipApplicationsList ?? [];

  const tabs = [
    {
      id: 'info',
      title: 'Informace',
      contents: () => (
        <>
          <RichTextView value={club.description} />

          {club.tenantLocationsList.map((item) => (
            <ActionRow key={item.id} actions={locationActionMap.get(item.id)!}>
              <Link
                className="grow py-1 text-sm font-bold underline"
                href={`/lokality/${item.id}`}
              >
                {item.name}
              </Link>
            </ActionRow>
          ))}
        </>
      ),
    },
    {
      id: 'trainers',
      title: `Trenéři (${club.tenantTrainersList.length})`,
      contents: () => (
        <>
          {club.tenantTrainersList.map((item) => (
            <ActionRow key={item.id} actions={trainerActionMap.get(item.id)!}>
              <div className="grow gap-3 align-baseline flex flex-wrap justify-between text-sm py-1">
                {!item.person ? (
                  '?'
                ) : (
                  <Link
                    className="underline font-bold grow basis-40"
                    href={`/clenove/${item.person.id}`}
                  >
                    {item.person.name}
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
                  </>
                )}
              </div>
            </ActionRow>
          ))}
        </>
      ),
    },
    ...(auth.isAdmin
      ? [
          {
            id: 'administrators',
            title: `Správci (${club.tenantAdministratorsList.length})`,
            contents: () => (
              <>
                {club.tenantAdministratorsList.map((item) => (
                  <ActionRow key={item.id} actions={administratorActionMap.get(item.id)!}>
                    {!item.person ? (
                      '?'
                    ) : (
                      <Link
                        className="underline font-bold text-sm py-1"
                        href={`/clenove/${item.person.id}`}
                      >
                        {item.person.name}
                      </Link>
                    )}
                  </ActionRow>
                ))}
              </>
            ),
          },
          ...(pendingApplications.length > 0
            ? [
                {
                  id: 'applications',
                  title: `Žádosti o členství (${pendingApplications.length})`,
                  contents: () => (
                    <>
                      {pendingApplications.map((item) => (
                        <Dialog key={item.id}>
                          <DialogTrigger.Edit
                            className="my-2 justify-start"
                            text={`${item.firstName} ${item.lastName}`}
                          />
                          <DialogContent>
                            <CreateMembershipApplicationForm data={item} />
                          </DialogContent>
                        </Dialog>
                      ))}
                    </>
                  ),
                },
              ]
            : []),
          {
            id: 'csts',
            title: 'ČSTS IDT',
            contents: () => <CstsIdBackfillWidget />,
          },
        ]
      : []),
  ];

  return (
    <>
      <PageHeader title="Klub" actions={tenantActions} />
      <TabMenu selected={tab} onSelect={setTab} options={tabs} />
    </>
  );
}
