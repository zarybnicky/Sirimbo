import { Control, useFieldArray } from "react-hook-form";
import { TypeOf } from "zod";
import { EventForm } from "./types";
import React from "react";
import { useQuery } from "urql";
import { CurrentTenantDocument } from "@/graphql/Tenant";
import { buttonCls } from "../style";
import { Popover, PopoverTrigger } from '@/ui/popover';
import * as PopoverPrimitive from '@radix-ui/react-popover';
import { Plus, X } from 'lucide-react';
import { ComboboxSearchArea } from "../Combobox";
import { formatLongCoupleName } from "../format";

export function ParticipantListElement({ name, control }: {
  control: Control<TypeOf<typeof EventForm>>;
  name: 'registrations';
}) {
  const [open, setOpen] = React.useState<'couple' | 'person' | null>(null);
  const { fields, append, remove, update } = useFieldArray({ name, control });

  const [tenantQuery] = useQuery({ query: CurrentTenantDocument });

  const possibleCouples = React.useMemo(() => (tenantQuery.data?.tenant?.couplesList || []).filter(x => x.active).map((c) => ({
    id: c.id,
    label: formatLongCoupleName(c),
  })), [tenantQuery]);

  const possiblePeople = (tenantQuery.data?.tenant?.tenantMembershipsList || []).filter(x => x.active).map((p) => ({
    id: p.person?.id ?? '',
    label: p.person?.name || '?',
  }));

  const activeFields = fields.filter(x => x.personId || x.coupleId);

  return (
    <>
      <div className="flex flex-wrap items-baseline gap-2 pt-1">
        <div className="grow"><b>Účastníci ({activeFields.length})</b></div>

        <Popover open={open === 'couple'} onOpenChange={(x) => setOpen(x ? 'couple' : null)}>
          <PopoverTrigger asChild>
            <button type="button" className={buttonCls({ size: 'xs', variant: 'outline' })}>
              <Plus /> Pár
            </button>
          </PopoverTrigger>
          <PopoverPrimitive.Portal>
            <PopoverPrimitive.Content className="z-40 PopoverContent" align="end" side='top' sideOffset={5}>
              <ComboboxSearchArea
                value={null}
                options={possibleCouples}
                onChange={(id) => {
                  if (id) append({ personId: null, coupleId: id, isConfirmed: true })
                  setOpen(null)
                }}
              />
            </PopoverPrimitive.Content>
          </PopoverPrimitive.Portal>
        </Popover>

        <Popover open={open === 'person'} onOpenChange={(x) => setOpen(x ? 'person' : null)}>
          <PopoverTrigger asChild>
            <button type="button" className={buttonCls({ size: 'xs', variant: 'outline' })}>
              <Plus /> Člověk
            </button>
          </PopoverTrigger>
          <PopoverPrimitive.Portal>
            <PopoverPrimitive.Content className="z-40 PopoverContent" align="end" side='top' sideOffset={5}>
              <ComboboxSearchArea
                value={null}
                options={possiblePeople}
                onChange={(id) => {
                  if (id) append({ personId: id, coupleId: null, isConfirmed: true })
                  setOpen(null)
                }}
              />
            </PopoverPrimitive.Content>
          </PopoverPrimitive.Portal>
        </Popover>
      </div>

      <div className={"grid gap-x-2 gap-y-1" + (fields.length > 6 ? ' grid-cols-2' : '')}>
        {fields.map((registration, index) => (
          <div className="flex items-center gap-2" key={registration.id}>
            <div className="grow">
              {registration.personId
              ? possiblePeople.find(x => x.id === registration.personId)?.label
              : possibleCouples.find(x => x.id === registration.coupleId)?.label}
            </div>
            <button
              type="button"
              className={buttonCls({ size: 'sm', variant: 'outline' })}
              onClick={() => registration.itemId ? update(index, { ...registration, personId: null, coupleId: null }) : remove(index)}
            >
              <X />
            </button>
          </div>
        ))}
      </div>

    </>
  );
}
