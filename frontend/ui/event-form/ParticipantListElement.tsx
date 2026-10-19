import { useFormContext, useWatch } from 'react-hook-form';
import type { EventFormInput, EventFormType, EventParticipantOption } from './types';
import { buttonCls } from '@/ui/style';
import { Plus, X } from 'lucide-react';
import { ComboboxButton } from '@/ui/fields/Combobox';
import { cn } from '@/lib/cn';
import type { EventFormOptionsQuery } from '@/graphql/Event';
import { CohortListElement } from './CohortListElement';

export function ParticipantListElement({
  options,
  cohorts,
}: {
  options: Map<string, EventParticipantOption>;
  cohorts: NonNullable<EventFormOptionsQuery['tenant']>['cohortsList'];
}) {
  const { control, setValue } = useFormContext<EventFormInput, unknown, EventFormType>();
  const registrations = useWatch({ control, name: 'registrations' }) ?? [];

  const active = registrations.filter((x) => !x.isCancelled);
  const selected = new Set(
    active.map(({ personId, coupleId }) =>
      personId ? `person:${personId}` : `couple:${coupleId}`,
    ),
  );
  const couplePeople = new Set(
    active.flatMap((r) => r.coupleId ? options.get(`couple:${r.coupleId}`)?.personIds ?? [] : []),
  );

  const addCohort = (cohortId: string) => {
    const next = [...registrations];
    const now = Date.now();
    const cohort = cohorts.find((x) => x.id === cohortId);
    for (const membership of cohort?.cohortMembershipsList ?? []) {
      const personId = membership.person?.id;
      if (
        !personId || couplePeople.has(personId) || membership.status !== 'ACTIVE' ||
        Date.parse(membership.since) > now ||
        (membership.until && Date.parse(membership.until) <= now)
      ) continue;
      const index = next.findIndex((x) => x.personId === personId);
      const current = next[index];
      if (current && !current.isCancelled && !current.cohortIds?.length) continue;
      const row = {
        personId, coupleId: null, isCancelled: false,
        cohortIds: [...new Set([...(current?.isCancelled ? [] : current?.cohortIds ?? []), cohortId])],
      };
      if (index === -1) next.push(row);
      else next[index] = row;
    }
    setValue('registrations', next, { shouldDirty: true });
  };

  const removeCohort = (cohortId: string) => {
    setValue('registrations', registrations.flatMap((r) => {
      if (!r.cohortIds?.includes(cohortId)) return [r];
      const cohortIds = r.cohortIds.filter((id) => id !== cohortId);
      return cohortIds.length > 0 ? [{ ...r, cohortIds }] : [];
    }), { shouldDirty: true });
  };

  const change = (id: string, add: boolean) => {
    const participant = options.get(id);
    if (!participant) return;
    const { personId, coupleId, personIds } = participant;
    const next = registrations.filter(
      (x) => x.personId ? !personIds.includes(x.personId) : x.coupleId !== coupleId,
    );
    next.push({ personId, coupleId, cohortIds: [], isCancelled: !add });
    if (!add && coupleId) {
      next.push(...personIds.map((personId) => ({
        personId, coupleId: null, cohortIds: [], isCancelled: true,
      })));
    }
    setValue('registrations', next, { shouldDirty: true });
  };

  return (
    <>
      <CohortListElement cohorts={cohorts} onAdd={addCohort} onRemove={removeCohort} />
      <div className="flex flex-wrap items-baseline gap-2 pt-1">
        <b className="grow">Účastníci ({selected.size})</b>
        {(['couple', 'person'] as const).map((kind) => (
          <ComboboxButton
            key={kind}
            align="end"
            side="top"
            trigger={
              <button
                type="button"
                className={buttonCls({ size: 'xs', variant: 'outline' })}
              >
                <Plus /> {kind === 'couple' ? 'Pár' : 'Člověk'}
              </button>
            }
            options={[...options.values()].filter(
              (x) =>
                x.id.startsWith(`${kind}:`) &&
                !selected.has(x.id) &&
                !x.personIds.some((p) => couplePeople.has(p)),
            )}
            onChange={(id) => {
              if (id) change(id, true);
            }}
          />
        ))}
      </div>
      <div className={cn('grid gap-x-2 gap-y-1', selected.size > 6 && 'grid-cols-2')}>
        {active.map(({ personId, coupleId, cohortIds }) => {
          const id = personId ? `person:${personId}` : `couple:${coupleId}`;
          return (
            <div className="flex items-center gap-2" key={id}>
              <div className="grow">
                {options.get(id)?.label}
                {!!cohortIds?.length && (
                  <span className="ml-1 text-xs text-neutral-11">
                    ({cohortIds.map((id) => cohorts.find((x) => x.id === id)?.name).join(', ')})
                  </span>
                )}
              </div>
              <button
                type="button"
                aria-label={`Odebrat ${options.get(id)?.label ?? 'účastníka'}`}
                className={buttonCls({ size: 'sm', variant: 'outline' })}
                onClick={() => change(id, false)}
              >
                <X />
              </button>
            </div>
          );
        })}
      </div>
    </>
  );
}
