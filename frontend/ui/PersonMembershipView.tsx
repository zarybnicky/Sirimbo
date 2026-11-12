import { DotSeparated } from '@/ui/DotSeparated';
import type { CoupleFragment } from '@/graphql/Memberships';
import { memo } from 'react';
import type { PersonMembershipsQuery } from '@/graphql/Person';
import {
  formatOpenDateRange,
  fullDateFormatter,
  dateTimeFormatter,
  moneyFormatter,
  numericFullFormatter,
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
import { accessCredentialActions } from '@/lib/actions/accessCredential';
import { EntityLink } from './EntityLink';

export const PersonMembershipView = memo(function PersonMembershipView({
  item,
}: {
  item: NonNullable<PersonMembershipsQuery['person']>;
}) {
  const auth = useAuth();
  const { enableStarletImport } = useTenantConfig();
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
      {(item.couplesList.length > 0 ||
        actions.some((x) => x.id === 'person.createCouple')) && (
        <div className="flex justify-between items-baseline flex-wrap gap-x-2">
          <h3 className="text-lg font-semibold mt-4 mb-2">Páry</h3>

          <ActionGroup
            actions={actions.filter((x) => x.id === 'person.createCouple')}
            primary="person.createCouple"
            iconOnly
          />
        </div>
      )}

      {item.couplesList
        ?.toSorted((a, b) => a.since.localeCompare(b.since))
        ?.map((couple) => (
          <CoupleRow key={couple.id} item={couple} />
        ))}

      {(item.cohortMembershipsList.some(keyIsNonNull('cohort')) ||
        actions.some((x) => x.id === 'person.addToCohort')) && (
        <div className="flex justify-between items-baseline flex-wrap gap-x-2">
          <h3 className="text-lg font-semibold mt-4 mb-2">Tréninkové skupiny</h3>

          <ActionGroup
            actions={actions.filter((x) => x.id === 'person.addToCohort')}
            primary="person.addToCohort"
            iconOnly
          />
        </div>
      )}
      {item.cohortMembershipsList
        .filter(keyIsNonNull('cohort'))
        .toSorted((x, y) => x.since.localeCompare(y.since))
        .map((item) => (
          <ActionRow key={item.id} actions={cohortMembershipActionMap.get(item.id)!}>
            <div className="grow gap-2 align-baseline flex flex-wrap justify-between text-sm py-1">
              <EntityLink type="cohort" entity={item.cohort} className="font-bold" />
              <span>{formatOpenDateRange(item)}</span>
            </div>
          </ActionRow>
        ))}

      {(item.tenantMembershipsList.some(visibleMembership) ||
        item.tenantAdministratorsList.some(visibleMembership) ||
        item.tenantTrainersList.some(visibleMembership) ||
        actions.some((x) =>
          ['person.addMember', 'person.addTrainer', 'person.addAdmin'].includes(x.id),
        )) && (
        <div className="flex justify-between items-baseline flex-wrap gap-x-2">
          <h3 className="text-lg font-semibold mt-4 mb-2">Členství</h3>
          <ActionGroup
            actions={actions.filter((x) =>
              ['person.addMember', 'person.addTrainer', 'person.addAdmin'].includes(x.id),
            )}
          />
        </div>
      )}

      {item.tenantAdministratorsList.filter(visibleMembership).map((item) => (
        <ActionRow key={item.id} actions={tenantAdministratorActionMap.get(item.id)!}>
          <div className="grow gap-2 align-baseline flex flex-wrap justify-between text-sm py-1">
            <span className="font-bold">{`Správce ${showClubs ? item.tenantName : ''}`}</span>
            {auth.isAdmin && <span>{formatOpenDateRange(item)}</span>}
          </div>
        </ActionRow>
      ))}

      {item.tenantTrainersList.filter(visibleMembership).map((item) => (
        <ActionRow key={item.id} actions={tenantTrainerActionMap.get(item.id)!}>
          <div className="grow gap-x-2 align-baseline flex flex-wrap justify-between text-sm py-1">
            <span className="font-bold">{`Trenér ${showClubs ? item.tenantName : ''}`}</span>
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
        <ActionRow key={item.id} actions={tenantMembershipActionMap.get(item.id)!}>
          <div className="grow gap-x-2 align-baseline flex flex-wrap justify-between text-sm py-1">
            <span className="font-bold">{`Člen ${showClubs ? item.tenantName : ''}`}</span>
            {auth.isAdmin && <span>{formatOpenDateRange(item)}</span>}
          </div>
        </ActionRow>
      ))}

      {(item.userProxiesList?.some(keyIsNonNull('user')) ||
        !!item.personInvitationsList?.length ||
        actions.some((x) => x.id === 'person.addAccess')) && (
        <div className="flex justify-between items-baseline gap-x-2">
          <h3 className="text-lg font-semibold mt-4 mb-2">Přístupové údaje</h3>
          <ActionGroup
            actions={actions.filter((x) => x.id === 'person.addAccess')}
            primary="person.addAccess"
            iconOnly
          />
        </div>
      )}

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

      {item.personInvitationsList?.map((invitation) => (
        <ActionRow key={invitation.id} actions={invitationActionMap.get(invitation.id)!}>
          <div className="grow flex flex-col sm:flex-row sm:items-baseline sm:justify-between gap-y-1 sm:gap-x-3 text-sm py-1">
            <b className="wrap-break-word">({invitation.email})</b>
            {invitation.lastSentAt && (
              <span>
                pozvánka odeslána {fullDateFormatter.format(new Date(invitation.lastSentAt))}
              </span>
            )}
          </div>
        </ActionRow>
      ))}

      {enableStarletImport && (
        <>
          {(item.accessCredentialsList.length > 0 ||
            actions.some((x) => x.id === 'person.addAccessCredential')) && (
            <div className="flex justify-between items-baseline flex-wrap gap-x-2">
              <h3 className="text-lg font-semibold mt-4 mb-2">Přístupové karty</h3>
              <ActionGroup
                actions={actions.filter((x) => x.id === 'person.addAccessCredential')}
                primary="person.addAccessCredential"
                iconOnly
              />
            </div>
          )}
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
                  {numericFullFormatter.format(new Date(event.occurredAt))}
                  {event.location && (
                    <Link className="underline" href={`/lokality/${event.location.id}`}>
                      {event.location.name}
                    </Link>
                  )}
                </DotSeparated>
              </div>
            ))}
          </div>
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
        <div className="grow gap-x-2 align-baseline flex flex-wrap justify-between text-sm py-1">
          <EntityLink type="couple" entity={item} className="font-bold" />
          <span>{formatOpenDateRange(item)}</span>
        </div>
      </ActionRow>
    </div>
  );
}
