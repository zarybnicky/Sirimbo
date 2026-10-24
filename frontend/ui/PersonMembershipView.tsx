import { DotSeparated } from '@/ui/DotSeparated';
import type { CoupleFragment } from '@/graphql/Memberships';
import { memo } from 'react';
import type { PersonMembershipsQuery } from '@/graphql/Person';
import {
  formatCoupleName,
  formatOpenDateRange,
  fullDateFormatter,
  dateTimeFormatter,
  moneyFormatter,
} from '@/ui/format';
import { useAuth, useTenantId, useTenantConfig } from '@/lib/auth';
import Link from 'next/link';
import { keyIsNonNull } from '@/lib/truthyFilter';
import { useActionMap, useActions } from '@/lib/actions';
import { cohortMembershipActions } from '@/lib/actions/cohortMembership';
import { coupleActions } from '@/lib/actions/couple';
import { personActions } from '@/lib/actions/person';
import { personInvitationActions } from '@/lib/actions/personInvitation';
import { tenantAdministratorActions } from '@/lib/actions/tenantAdministrator';
import { tenantMembershipActions } from '@/lib/actions/tenantMembership';
import { tenantTrainerActions } from '@/lib/actions/tenantTrainer';
import { userProxyActions } from '@/lib/actions/userProxy';
import { ActionRow } from '@/ui/ActionRow';
import { ActionGroup } from '@/ui/ActionGroup';
import { slugify } from '@/lib/slugify';
import { accessCredentialActions } from '@/lib/actions/accessCredential';

export const PersonMembershipView = memo(function PersonMembershipView({
  item,
}: {
  item: NonNullable<PersonMembershipsQuery['person']>;
}) {
  const auth = useAuth();
  const { enableStarletImport } = useTenantConfig();
  const isAdminOrCurrentPerson = auth.isAdmin || auth.isMyPerson(item.id);
  const tenantId = useTenantId();
  const showClubs =
    auth.isSystemAdmin || (auth.isAdmin && auth.adminTenantIds.length > 1);
  const visibleMembership = (membership: { tenantId: string }) =>
    membership.tenantId === tenantId ||
    auth.isSystemAdmin ||
    (auth.isAdmin && auth.adminTenantIds.includes(membership.tenantId));
  const actions = useActions(personActions, item);
  const cohortMembershipActionMap = useActionMap(
    cohortMembershipActions,
    item.cohortMembershipsList,
  );
  const tenantMembershipActionMap = useActionMap(
    tenantMembershipActions,
    item.tenantMembershipsList,
  );
  const tenantAdministratorActionMap = useActionMap(
    tenantAdministratorActions,
    item.tenantAdministratorsList,
  );
  const tenantTrainerActionMap = useActionMap(
    tenantTrainerActions,
    item.tenantTrainersList,
  );
  const userProxyActionMap = useActionMap(userProxyActions, item.userProxiesList ?? []);
  const credentialActionMap = useActionMap(
    accessCredentialActions,
    item.accessCredentialsList,
  );
  const invitationActionMap = useActionMap(
    personInvitationActions,
    item.personInvitationsList ?? [],
  );

  return (
    <div key="info" className="mb-2">
      <div className="flex justify-between items-baseline flex-wrap gap-4">
        <h3 className="text-lg font-semibold mt-4 mb-2">Páry</h3>

        <ActionGroup
          actions={actions.filter((action) => action.id === 'person.createCouple')}
          primary="person.createCouple"
          iconOnly
        />
      </div>

      {item.allCouplesList
        ?.toSorted((a, b) => a.since.localeCompare(b.since))
        ?.map((couple) => (
          <CoupleRow key={couple.id} item={couple} />
        ))}

      <div className="flex justify-between items-baseline flex-wrap gap-4">
        <h3 className="text-lg font-semibold mt-4 mb-2">Tréninkové skupiny</h3>

        <ActionGroup
          actions={actions.filter((action) => action.id === 'person.addToCohort')}
          primary="person.addToCohort"
          iconOnly
        />
      </div>
      {item.cohortMembershipsList
        .filter(keyIsNonNull('cohort'))
        .toSorted((x, y) => x.since.localeCompare(y.since))
        .map((item) => (
          <ActionRow key={item.id} actions={cohortMembershipActionMap.get(item.id)!}>
            <div className="grow gap-2 align-baseline flex flex-wrap justify-between text-sm py-1">
              <Link
                className="underline font-bold"
                href={`/treninkove-skupiny/${item.cohort.id}/${slugify(item.cohort.name)}`}
              >
                {item.cohort.name}
              </Link>
              <span>{formatOpenDateRange(item)}</span>
            </div>
          </ActionRow>
        ))}

      <div className="flex justify-between items-baseline flex-wrap gap-4">
        <h3 className="text-lg font-semibold mt-4 mb-2">Členství</h3>
        <ActionGroup
          actions={actions.filter((action) =>
            ['person.addMember', 'person.addTrainer', 'person.addAdmin'].includes(
              action.id,
            ),
          )}
        />
      </div>

      {item.tenantAdministratorsList.filter(visibleMembership).map((item) => (
        <ActionRow
          key={item.id}
          actions={
            auth.isSystemAdmin || item.tenantId === tenantId
              ? tenantAdministratorActionMap.get(item.id)!
              : []
          }
        >
          <div className="grow gap-2 align-baseline flex flex-wrap justify-between text-sm py-1">
            <DotSeparated className="font-bold">
              Správce{showClubs && item.tenantName}
            </DotSeparated>
            {auth.isAdmin && <span>{formatOpenDateRange(item)}</span>}
          </div>
        </ActionRow>
      ))}

      {item.tenantTrainersList.filter(visibleMembership).map((item) => (
        <ActionRow
          key={item.id}
          actions={
            auth.isSystemAdmin || item.tenantId === tenantId
              ? tenantTrainerActionMap.get(item.id)!
              : []
          }
        >
          <div className="grow gap-4 align-baseline flex flex-wrap justify-between text-sm py-1">
            <DotSeparated className="font-bold">
              Trenér{showClubs && item.tenantName}
            </DotSeparated>
            {auth.isAdmin && (
              <>
                <div>
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
                <span>{formatOpenDateRange(item)}</span>
              </>
            )}
          </div>
        </ActionRow>
      ))}

      {item.tenantMembershipsList.filter(visibleMembership).map((item) => (
        <ActionRow
          key={item.id}
          actions={
            auth.isSystemAdmin || item.tenantId === tenantId
              ? tenantMembershipActionMap.get(item.id)!
              : []
          }
        >
          <div className="grow gap-4 align-baseline flex flex-wrap justify-between text-sm py-1">
            <DotSeparated className="font-bold">
              Člen{showClubs && item.tenantName}
            </DotSeparated>
            {auth.isAdmin && <span>{formatOpenDateRange(item)}</span>}
          </div>
        </ActionRow>
      ))}

      {isAdminOrCurrentPerson && (
        <>
          <div className="flex justify-between items-baseline flex-wrap gap-4">
            <h3 className="text-lg font-semibold mt-4 mb-2">Přístupové údaje</h3>
            {auth.isAdmin && (
              <ActionGroup
                actions={actions.filter((action) =>
                  ['person.invite', 'person.linkUser'].includes(action.id),
                )}
                groups={{ add: ['person.invite', 'person.linkUser'] }}
              />
            )}
          </div>

          {item.userProxiesList?.filter(keyIsNonNull('user')).map((proxy) => (
            <ActionRow key={proxy.id} actions={userProxyActionMap.get(proxy.id)!}>
              <div className="grow flex flex-col sm:flex-row sm:items-baseline sm:justify-between text-sm gap-y-1 sm:gap-x-3">
                <b className="wrap-break-word">
                  <Link href={`/users/${proxy.user.id}`} className="underline">
                    {[proxy.user.uEmail, proxy.user.uLogin].filter(Boolean).join(', ')}
                  </Link>
                </b>
                <span>{formatOpenDateRange(proxy)}</span>
              </div>
            </ActionRow>
          ))}

          {auth.isAdmin &&
            item.personInvitationsList?.map((invitation) => (
              <ActionRow
                key={invitation.id}
                actions={invitationActionMap.get(invitation.id)!}
              >
                <div className="grow flex flex-col sm:flex-row sm:items-baseline sm:justify-between gap-y-1 sm:gap-x-3 text-sm py-1">
                  <b className="wrap-break-word">({invitation.email})</b>
                  <span>
                    pozvánka odeslána{' '}
                    {fullDateFormatter.format(new Date(invitation.createdAt))}
                  </span>
                </div>
              </ActionRow>
            ))}

          {auth.isAdmin && enableStarletImport && (
            <>
              <div className="flex justify-between items-baseline flex-wrap gap-4">
                <h3 className="text-lg font-semibold mt-4 mb-2">Přístupové karty</h3>
                <ActionGroup
                  actions={actions.filter(
                    (action) => action.id === 'person.addAccessCredential',
                  )}
                  primary="person.addAccessCredential"
                  iconOnly
                />
              </div>
              {item.accessCredentialsList.map((card) => (
                <ActionRow key={card.id} actions={credentialActionMap.get(card.id)!}>
                  <div className="grow gap-3 align-baseline flex flex-wrap justify-between text-sm py-1">
                    <span className={card.isAllowed ? '' : 'line-through'}>
                      <b>{card.label}</b>
                      <code className="ml-2 text-neutral-11">{card.code}</code>
                    </span>
                    <span className="text-right text-neutral-11">
                      {card.lastUsed
                        ? `Naposledy použita ${dateTimeFormatter.format(new Date(card.lastUsed))}`
                        : 'Nikdy nepoužita'}
                    </span>
                  </div>
                </ActionRow>
              ))}
            </>
          )}

          {item.accessEventsList.length > 0 && (
            <>
              <h3 className="text-lg font-semibold mt-4 mb-2">Poslední průchody</h3>
              <div className="divide-y divide-neutral-5 rounded-md border border-neutral-5">
                {item.accessEventsList.map((event) => (
                  <div
                    key={event.id}
                    className="flex flex-wrap justify-between gap-2 px-3 py-2 text-sm"
                  >
                    <b>{event.allowed ? 'Povoleno' : 'Zamítnuto'}</b>
                    <DotSeparated className="text-neutral-11">
                      {dateTimeFormatter.format(new Date(event.occurredAt))}
                      {event.location && (
                        <Link
                          className="underline"
                          href={`/lokality/${event.location.id}`}
                        >
                          {event.location.name}
                        </Link>
                      )}
                      {event.device}
                    </DotSeparated>
                  </div>
                ))}
              </div>
            </>
          )}
        </>
      )}
    </div>
  );
});

function CoupleRow({ item }: { item: CoupleFragment }) {
  const actions = useActions(coupleActions, item);
  return (
    <div>
      <ActionRow actions={actions}>
        <div className="grow gap-2 align-baseline flex flex-wrap justify-between text-sm py-1">
          <Link className="underline font-bold" href={`/pary/${item.id}`}>
            {formatCoupleName(item)}
          </Link>
          <span>{formatOpenDateRange(item)}</span>
        </div>
      </ActionRow>
    </div>
  );
}
