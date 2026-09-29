import { useFormContext, useWatch } from 'react-hook-form';
import type { EventFormInput, EventFormType } from './types';
import React from 'react';
import { buttonCls } from '@/ui/style';
import { Popover, PopoverTrigger } from '@/ui/popover';
import * as PopoverPrimitive from '@radix-ui/react-popover';
import { Plus, X } from 'lucide-react';
import { ComboboxSearchArea } from '@/ui/fields/Combobox';
import { formatCoupleName } from '@/ui/format';
import { cn } from '@/lib/cn';
import { useQuery } from 'urql';
import {
  EventFormOptionsDocument,
  type EventInstanceRegistrationFragment,
} from '@/graphql/Event';
import { FormError } from '@/ui/form';
import { isTruthy } from '@/lib/truthyFilter';
import { CohortListElement } from './CohortListElement';

export function ParticipantListElement({
  savedRegistrations = [],
}: {
  savedRegistrations?: EventInstanceRegistrationFragment[];
}) {
  const { control, setValue } = useFormContext<EventFormInput, unknown, EventFormType>();
  const registrations = useWatch({ control, name: 'registrations' }) ?? [];
  const [open, setOpen] = React.useState<'person' | 'couple' | null>(null);
  const [{ data, error }] = useQuery({ query: EventFormOptionsDocument });

  // Saved participants remain available even after leaving the club or a couple.
  const options = new Map<
    string,
    {
      id: string;
      label: string;
      personId: string | null;
      coupleId: string | null;
      personIds: string[];
    }
  >();
  for (const person of [
    ...savedRegistrations.map(({ person }) => person),
    ...(data?.tenant?.cohortsList ?? []).flatMap((cohort) =>
      cohort.cohortMembershipsList.map((x) => x.person),
    ),
    ...(data?.tenant?.tenantMembershipsList ?? []).map((x) => x.person),
  ]) {
    if (!person) continue;
    options.set(`person:${person.id}`, {
      id: `person:${person.id}`,
      label: person.name,
      personId: person.id,
      coupleId: null,
      personIds: [person.id],
    });
  }
  for (const couple of [
    ...savedRegistrations.map((x) => x.couple),
    ...(data?.tenant?.couplesList ?? []).filter((c) => c.status === 'ACTIVE'),
  ]) {
    if (!couple) continue;
    options.set(`couple:${couple.id}`, {
      id: `couple:${couple.id}`,
      label: formatCoupleName(couple),
      personId: null,
      coupleId: couple.id,
      personIds: [couple.man?.id, couple.woman?.id].filter(isTruthy),
    });
  }

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
    const cohort = data?.tenant?.cohortsList.find((x) => x.id === cohortId);
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
    setOpen(null);
  };

  return (
    <>
      <CohortListElement onAdd={addCohort} onRemove={removeCohort} />
      <div className="flex flex-wrap items-baseline gap-2 pt-1">
        <b className="grow">Účastníci ({selected.size})</b>
        {(['couple', 'person'] as const).map((kind) => (
          <Popover
            key={kind}
            open={open === kind}
            onOpenChange={(value) => setOpen(value ? kind : null)}
          >
            <PopoverTrigger asChild>
              <button
                type="button"
                className={buttonCls({ size: 'xs', variant: 'outline' })}
              >
                <Plus /> {kind === 'couple' ? 'Pár' : 'Člověk'}
              </button>
            </PopoverTrigger>
            <PopoverPrimitive.Portal>
              <PopoverPrimitive.Content
                className="z-40 max-h-(--radix-popover-content-available-height)"
                align="end"
                side="top"
                sideOffset={5}
              >
                <ComboboxSearchArea
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
              </PopoverPrimitive.Content>
            </PopoverPrimitive.Portal>
          </Popover>
        ))}
      </div>
      <FormError error={error} />
      <div className={cn('grid gap-x-2 gap-y-1', selected.size > 6 && 'grid-cols-2')}>
        {active.map(({ personId, coupleId, cohortIds }) => {
          const id = personId ? `person:${personId}` : `couple:${coupleId}`;
          return (
            <div className="flex items-center gap-2" key={id}>
              <div className="grow">
                {options.get(id)?.label}
                {!!cohortIds?.length && (
                  <span className="ml-1 text-xs text-neutral-11">
                    ({cohortIds.map((id) => data?.tenant?.cohortsList.find((x) => x.id === id)?.name).join(', ')})
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
