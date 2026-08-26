import { useFieldArray } from 'react-hook-form';
import type { EventFormControl } from '@/ui/event-form/types';
import React from 'react';
import { buttonCls } from '@/ui/style';
import { Popover, PopoverTrigger } from '@/ui/popover';
import * as PopoverPrimitive from '@radix-ui/react-popover';
import { Plus, X } from 'lucide-react';
import { ComboboxSearchArea } from '@/ui/fields/Combobox';
import { formatCoupleName } from '@/ui/format';
import { cn } from '@/lib/cn';
import { useQuery } from 'urql';
import { CurrentTenantDocument } from '@/graphql/Tenant';

export function ParticipantListElement({
  control,
  existingPeople = [],
  existingCouples = [],
}: {
  control: EventFormControl;
  existingPeople?: { id: string; label: string }[];
  existingCouples?: { id: string; label: string }[];
}) {
  const [open, setOpen] = React.useState<'couple' | 'person' | null>(null);
  const { fields, append, remove } = useFieldArray({ name: 'registrations', control });
  const [{ data: tenant }] = useQuery({ query: CurrentTenantDocument });

  const possibleCouples = React.useMemo(
    () =>
      (tenant?.tenant?.couplesList ?? []).flatMap((couple) =>
        couple.status === 'ACTIVE'
          ? [{ id: couple.id, label: formatCoupleName(couple) }]
          : [],
      ),
    [tenant],
  );

  const possiblePeople = React.useMemo(
    () =>
      (tenant?.tenant?.tenantMembershipsList ?? []).flatMap(({ person, status }) =>
        status === 'ACTIVE' && person ? [{ id: person.id, label: person.name }] : [],
      ),
    [tenant],
  );

  const selectCouple = React.useCallback(
    (id: string | null | undefined) => {
      if (id) append({ personId: null, coupleId: id });
      setOpen(null);
    },
    [append],
  );

  const selectPerson = React.useCallback(
    (id: string | null | undefined) => {
      if (id) append({ personId: id, coupleId: null });
      setOpen(null);
    },
    [append],
  );

  return (
    <>
      <div className="flex flex-wrap items-baseline gap-2 pt-1">
        <b className="grow">Účastníci ({fields.length})</b>

        <Popover
          open={open === 'couple'}
          onOpenChange={(x) => setOpen(x ? 'couple' : null)}
        >
          <PopoverTrigger asChild>
            <button
              type="button"
              className={buttonCls({ size: 'xs', variant: 'outline' })}
            >
              <Plus /> Pár
            </button>
          </PopoverTrigger>
          <PopoverPrimitive.Portal>
            <PopoverPrimitive.Content
              className="z-40 max-h-(--radix-popover-content-available-height)"
              align="end"
              side="top"
              sideOffset={5}
            >
              <ComboboxSearchArea options={possibleCouples} onChange={selectCouple} />
            </PopoverPrimitive.Content>
          </PopoverPrimitive.Portal>
        </Popover>

        <Popover
          open={open === 'person'}
          onOpenChange={(x) => setOpen(x ? 'person' : null)}
        >
          <PopoverTrigger asChild>
            <button
              type="button"
              className={buttonCls({ size: 'xs', variant: 'outline' })}
            >
              <Plus /> Člověk
            </button>
          </PopoverTrigger>
          <PopoverPrimitive.Portal>
            <PopoverPrimitive.Content
              className="z-40 max-h-(--radix-popover-content-available-height)"
              align="end"
              side="top"
              sideOffset={5}
            >
              <ComboboxSearchArea options={possiblePeople} onChange={selectPerson} />
            </PopoverPrimitive.Content>
          </PopoverPrimitive.Portal>
        </Popover>
      </div>

      <div className={cn('grid gap-x-2 gap-y-1', fields.length > 6 && 'grid-cols-2')}>
        {fields.map((registration, index) => {
          const label = registration.personId
            ? (possiblePeople.find((x) => x.id === registration.personId)?.label ??
              existingPeople.find((x) => x.id === registration.personId)?.label)
            : (possibleCouples.find((x) => x.id === registration.coupleId)?.label ??
              existingCouples.find((x) => x.id === registration.coupleId)?.label);

          return (
            <div className="flex items-center gap-2" key={registration.id}>
              <div className="grow">{label}</div>
              <button
                type="button"
                aria-label={`Odebrat ${label ?? 'účastníka'}`}
                className={buttonCls({ size: 'sm', variant: 'outline' })}
                onClick={() => remove(index)}
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
