'use client';

import {
  type MembershipApplicationFragment,
  MembershipApplicationsDocument,
} from '@/graphql/MembershipApplication';
import { PageHeader } from '@/ui/PageHeader';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { formatAddress, moneyFormatter } from '@/ui/format';
import { MembershipApplicationForm } from '@/ui/forms/MembershipApplicationForm.tsx';
import { LocationForm } from '@/ui/forms/LocationForm';
import { useAuth } from '@/lib/auth';
import { PinIcon } from 'lucide-react';
import Link from 'next/link';
import Image from '@/ui/Image';
import { useQuery } from 'urql';
import { ClubDocument, type ClubQuery } from '@/graphql/Tenant';
import { useActionMap, useActions } from '@/lib/actions';
import { tenantActions } from '@/lib/actions/tenant';
import { tenantAdministratorActions } from '@/lib/actions/tenantAdministrator';
import { locationActions } from '@/lib/actions/location';
import { tenantTrainerActions } from '@/lib/actions/tenantTrainer';
import { ActionRow } from '@/ui/ActionRow';
import { FederationIdWidget } from './FederationIdWidget.tsx';
import { badgeCls } from '@/ui/style';
import { CompetitionSuggestions } from '@/ui/Competitions';
import { Tab, TabMenu } from '@/ui/TabMenu';
import { parseAsString, useQueryState } from 'nuqs';
import React from 'react';
import { EntityLink } from '@/ui/EntityLink.tsx';
import { keyIsNonNull } from '@/lib/truthyFilter.ts';
import { DefinitionList } from '@/ui/DefinitionList';
import { RichTextView } from '@/ui/RichTextView';

type ClubData = NonNullable<ClubQuery['tenant']>;

export function Club() {
  const auth = useAuth();
  const [tab, setTab] = useQueryState(
    'tab',
    parseAsString.withOptions({ history: 'push' }),
  );
  const [{ data: tenant }] = useQuery({
    query: ClubDocument,
    variables: {
      showInLists: auth.isAdmin ? undefined : true,
    },
  });
  const [{ data: applications }] = useQuery({
    query: MembershipApplicationsDocument,
    variables: { condition: { status: 'SENT' } },
    pause: !auth.isAdmin,
  });
  const actions = useActions(tenantActions, tenant?.tenant ? tenant.tenant : null);

  if (!tenant?.tenant) return null;
  const club = tenant.tenant;
  const pendingApplications = applications?.membershipApplicationsList ?? [];

  return (
    <>
      <PageHeader title={club.name} actions={actions} />
      <TabMenu selected={tab} onSelect={setTab}>
        <Tab id="info" title="Informace">
          <ClubInformation club={club} />
        </Tab>
        <Tab id="trainers" title={`Trenéři (${club.tenantTrainersList.length})`}>
          <ClubTrainers club={club} />
        </Tab>
        <Tab
          id="administrators"
          title={`Správci (${club.tenantAdministratorsList.length})`}
          requireAdmin
        >
          <ClubAdministrators club={club} />
        </Tab>
        {pendingApplications.length > 0 && (
          <Tab
            id="applications"
            title={`Žádosti o členství (${pendingApplications.length})`}
            requireAdmin
          >
            <ClubApplications applications={pendingApplications} />
          </Tab>
        )}
        <Tab id="eligible" title="Možné soutěže">
          <CompetitionSuggestions />
        </Tab>
        <Tab id="csts" title="ČSTS IDT" requireAdmin>
          <FederationIdWidget federation="csts" />
        </Tab>
        <Tab id="wdsf" title="WDSF MIN" requireAdmin>
          <FederationIdWidget federation="wdsf" />
        </Tab>
      </TabMenu>
    </>
  );
}

const ClubInformation = React.memo(function ClubInformation({
  club,
}: {
  club: ClubData;
}) {
  const auth = useAuth();
  const locationActionMap = useActionMap(locationActions, club.locationsList);
  return (
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
                <span className="flex min-w-0 items-center gap-2 text-neutral-11">
                  {auth.isAdmin && !item.showInLists && (
                    <span className={badgeCls({ className: 'shrink-0' })}>Skryté</span>
                  )}
                  <span className="truncate">{formatAddress(item.address)}</span>
                </span>
              </span>
            </Link>
          </ActionRow>
        ))
      ) : (
        <p className="text-sm text-neutral-10">Nejsou přidána žádná místa.</p>
      )}

      {auth.isAdmin && (
        <>
          <h2 className="mt-2 text-lg font-bold">Detaily</h2>
          <DefinitionList
            items={{
              ID: club.id,
              Členové: club.memberships.totalCount,
              Trenéři: club.trainers.totalCount,
              Správci: club.administrators.totalCount,
              'Bankovní účet': club.bankAccount,
              'ČSTS ID': club.cstsId,
              Domény: club.origins.join(', '),
              IČO: club.czIco,
              DIČ: club.czDic,
              Adresa: formatAddress(club.address),
              Nastavení: auth.isSystemAdmin && (
                <pre className="overflow-auto whitespace-pre-wrap wrap-anywhere">
                  {JSON.stringify(club.tenantSetting?.settings ?? {}, null, 2)}
                </pre>
              ),
            }}
          />
        </>
      )}
    </>
  );
});

const ClubTrainers = React.memo(function ClubTrainers({ club }: { club: ClubData }) {
  const auth = useAuth();
  const trainerActionMap = useActionMap(tenantTrainerActions, club.tenantTrainersList);
  return (
    <>
      {club.tenantTrainersList.filter(keyIsNonNull('person')).map((item) => (
        <ActionRow key={item.id} actions={trainerActionMap.get(item.id)!}>
          <div className="grow gap-3 align-baseline flex flex-wrap justify-between text-sm py-1">
            <EntityLink
              type="person"
              entity={item.person}
              className="font-bold text-sm py-1"
            />
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
  );
});

const ClubAdministrators = React.memo(function ClubAdministrators({
  club,
}: {
  club: ClubData;
}) {
  const administratorActionMap = useActionMap(
    tenantAdministratorActions,
    club.tenantAdministratorsList,
  );
  return (
    <>
      {club.tenantAdministratorsList.filter(keyIsNonNull('person')).map((item) => (
        <ActionRow key={item.id} actions={administratorActionMap.get(item.id)!}>
          <EntityLink
            type="person"
            entity={item.person}
            className="font-bold text-sm py-1"
          />
        </ActionRow>
      ))}
    </>
  );
});

const ClubApplications = React.memo(function ClubApplications({
  applications,
}: {
  applications: MembershipApplicationFragment[];
}) {
  return (
    <>
      {applications.map((item) => (
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
  );
});
