'use client';

import { useState, type ReactNode } from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { z } from 'zod';
import { Popover, PopoverContent, PopoverTrigger } from '@/ui/popover';
import { TextFieldElement } from '@/ui/fields/text';
import { ComboboxElement } from '@/ui/fields/Combobox';
import { FormError } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import { cn } from '@/lib/cn';
import type { IGetRefetchPoliciesResult } from './crawler.queries';
import { createRefetchPolicy, removeRefetchPolicy, updateRefetchPolicy } from './actions';

const units = [
  'seconds',
  'minutes',
  'hours',
  'days',
  'weeks',
  'months',
  'years',
] as const;
const unitOptions = units.map((id) => ({ id, label: id }));
type IntervalInput = { amount: number; unit: (typeof units)[number] };
const Form = z.object({
  amount: z.int().positive(),
  unit: z.enum(units),
  offsetAmount: z.int(),
  offsetUnit: z.enum(units),
});

function intervalInput(interval: string): IntervalInput | null {
  // PostgreSQL returns calendar intervals as text and sub-day intervals as a clock.
  const months = /^(-?\d+) years? (-?\d+) mons?$/.exec(interval);
  if (months)
    return { amount: Number(months[1]) * 12 + Number(months[2]), unit: 'months' };
  const calendar = /^(-?\d+) (years?|mons?|days?)$/.exec(interval);
  if (calendar) {
    return {
      amount: Number(calendar[1]),
      unit: calendar[2]!.startsWith('year')
        ? 'years'
        : calendar[2]!.startsWith('mon')
          ? 'months'
          : 'days',
    };
  }
  const clock = /^(-?)(\d+):(\d+):(\d+)$/.exec(interval);
  if (!clock) return null;
  const seconds =
    (Number(clock[2]) * 3600 + Number(clock[3]) * 60 + Number(clock[4])) *
    (clock[1] ? -1 : 1);
  const divisor =
    seconds % 86_400 === 0
      ? 86_400
      : seconds % 3600 === 0
        ? 3600
        : seconds % 60 === 0
          ? 60
          : 1;
  return {
    amount: seconds / divisor,
    unit:
      divisor === 86_400
        ? 'days'
        : divisor === 3600
          ? 'hours'
          : divisor === 60
            ? 'minutes'
            : 'seconds',
  };
}

export function RefetchPolicyEditor({
  rule,
  create = false,
  className,
  children,
}: {
  rule: IGetRefetchPoliciesResult | undefined;
  create?: boolean;
  className?: string;
  children: ReactNode;
}) {
  const [open, setOpen] = useState(false);
  const interval: IntervalInput | null = create
    ? { amount: 1, unit: 'days' }
    : rule?.fetchInterval
      ? intervalInput(rule.fetchInterval)
      : null;
  const offset: IntervalInput | null =
    !create && rule?.offsetLabel
      ? intervalInput(rule.offsetLabel)
      : { amount: 0, unit: 'days' };
  if (!rule || !interval || !offset) return <span className={className}>{children}</span>;

  return (
    <Popover open={open} onOpenChange={setOpen}>
      <PopoverTrigger asChild>
        <button
          type="button"
          className={cn(
            !create &&
              'whitespace-nowrap underline decoration-dotted underline-offset-2 hover:text-accent-11',
            className,
          )}
        >
          {children}
        </button>
      </PopoverTrigger>
      <PopoverContent align="start" className="w-80 text-sm">
        <RefetchPolicyForm
          rule={rule}
          create={create}
          initial={{ ...interval, offsetAmount: offset.amount, offsetUnit: offset.unit }}
          onSuccess={() => setOpen(false)}
        />
      </PopoverContent>
    </Popover>
  );
}

function RefetchPolicyForm({
  rule,
  create,
  initial,
  onSuccess,
}: {
  rule: IGetRefetchPoliciesResult;
  create: boolean;
  initial: z.infer<typeof Form>;
  onSuccess: () => void;
}) {
  const {
    control,
    handleSubmit,
    setError,
    formState: { errors, isSubmitting },
  } = useForm<z.infer<typeof Form>>({
    resolver: zodResolver(Form),
    defaultValues: initial,
  });
  const onSubmit = async (values: z.infer<typeof Form>) => {
    try {
      if (create) {
        await createRefetchPolicy({
          federation: rule.federation!,
          kind: rule.kind!,
          ...values,
        });
      } else {
        await updateRefetchPolicy({
          federation: rule.federation!,
          kind: rule.kind!,
          offset: rule.offsetLabel,
          fetchInterval: rule.fetchInterval!,
          ...values,
        });
      }
      onSuccess();
    } catch (error) {
      setError('root.server', {
        message: error instanceof Error ? error.message : 'Could not save policy.',
      });
    }
  };
  const remove = useAsyncCallback(async () => {
    await removeRefetchPolicy({
      federation: rule.federation!,
      kind: rule.kind!,
      offset: rule.offsetLabel!,
    });
    onSuccess();
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit)}>
      <h4 className="pr-6 font-semibold">
        {create ? 'Add offset' : rule.offsetLabel === null ? 'Fetch interval' : 'Offset'}
      </h4>
      <FormError error={errors.root?.server?.message || remove.error} />
      {(create || rule.offsetLabel !== null) && (
        <div className="grid grid-cols-2 gap-2">
          <TextFieldElement
            control={control}
            name="offsetAmount"
            type="number"
            step="1"
            required
            label="Offset"
          />
          <ComboboxElement
            control={control}
            name="offsetUnit"
            label="Unit"
            options={unitOptions}
            searchable={false}
          />
        </div>
      )}
      <div className="grid grid-cols-2 gap-2">
        <TextFieldElement
          control={control}
          name="amount"
          type="number"
          step="1"
          required
          min={1}
          label="Fetch interval"
        />
        <ComboboxElement
          control={control}
          name="unit"
          label="Unit"
          options={unitOptions}
          searchable={false}
        />
      </div>
      {(create || rule.offsetLabel !== null) && (
        <p className="text-xs text-neutral-11">
          Offsets start at 00:00 UTC on the reference day. +1 day is the next midnight.
        </p>
      )}
      <div className="flex flex-wrap gap-4">
        <SubmitButton control={control} disabled={remove.loading}>
          Save
        </SubmitButton>
        {!create && rule.offsetLabel !== null && (
          <SubmitButton action={remove} variant="danger" disabled={isSubmitting}>
            Remove
          </SubmitButton>
        )}
      </div>
    </form>
  );
}
