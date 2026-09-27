'use client';

import { PendingMembershipApplicationsDocument } from '@/graphql/MembershipApplication';
import { RichTextView } from '@/ui/RichTextView';
import { PageHeader } from '@/ui/TitleBar';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { formatAddress, moneyFormatter } from '@/ui/format';
import { MembershipApplicationForm } from '@/ui/forms/MembershipApplicationForm.tsx';
import { LocationForm } from '@/ui/forms/LocationForm';
import { TenantForm } from '@/ui/forms/TenantForm.tsx';
import { useAuth } from '@/lib/auth';
import { Pencil, PinIcon } from 'lucide-react';
import Link from 'next/link';
import Image from 'next/image';
import { useQuery } from 'urql';
import { ClubDocument } from '@/graphql/Tenant';
import { useActionMap, useActions } from '@/lib/actions';
import { tenantAdministratorActions } from '@/lib/actions/tenantAdministrator';
import { locationActions } from '@/lib/actions/location';
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
  const [{ data: tenant }] = useQuery({ query: ClubDocument });
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
    locationActions,
    tenant?.tenant?.locationsList ?? [],
  );
  const tenantActions = useActions(
    [
      {
        id: 'tenant.edit',
        group: 'primary',
        label: 'Upravit klub',
        icon: Pencil,
        requireAdmin: true,
        render: () => <TenantForm />,
      },
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
          <h2 className="mb-2 text-lg font-bold">O klubu</h2>
          {club.description?.trim() ? (
            <RichTextView value={club.description} />
          ) : auth.isAdmin ? (
            <p className="text-sm text-neutral-10">(Nevyplněno)</p>
          ) : null}

          <div className="mt-4 flex flex-wrap items-baseline justify-between gap-4">
            <h2 className="mb-2 text-lg font-bold">Místa</h2>
            {auth.isAdmin && (
              <Dialog>
                <DialogTrigger.Add size="sm" />
                <DialogContent>
                  <LocationForm />
                </DialogContent>
              </Dialog>
            )}
          </div>

          {club.locationsList.length > 0 ? (
            club.locationsList.map((item) => (
              <ActionRow key={item.id} actions={locationActionMap.get(item.id)!}>
                <Link
                  className="flex min-w-0 items-center gap-3"
                  href={`/lokality/${item.id}`}
                >
                  <span className="relative flex h-14 w-20 shrink-0 items-center justify-center overflow-hidden rounded bg-neutral-3">
                    {item.coverImage ? (
                      <Image
                        fill
                        unoptimized
                        src={item.coverImage.url}
                        alt=""
                        sizes="80px"
                        className="object-cover"
                      />
                    ) : (
                      <PinIcon className="size-5 text-neutral-9" aria-hidden="true" />
                    )}
                  </span>
                  <span className="min-w-0 text-sm">
                    <span className="font-bold underline">{item.name}</span>
                    <span className="block truncate text-neutral-11">
                      {formatAddress(item.address)}
                    </span>
                  </span>
                </Link>
              </ActionRow>
            ))
          ) : (
            <p className="text-sm text-neutral-10">Nejsou přidána žádná místa.</p>
          )}
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
                    className="underline font-bold w-fit min-w-0"
                    href={`/clenove/${item.person.id}`}
                  >
                    {item.person.name}
                  </Link>
                )}
                {auth.isAdmin && (
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
                            <MembershipApplicationForm data={item} />
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
