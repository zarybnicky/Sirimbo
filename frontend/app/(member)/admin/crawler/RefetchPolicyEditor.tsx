'use client';

import { useState, type ReactNode } from 'react';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { z } from 'zod';
import { Popover, PopoverContent, PopoverTrigger } from '@/ui/popover';
import { TextFieldElement } from '@/ui/fields/text';
import { ComboboxElement } from '@/ui/fields/Combobox';
import { FormError } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import type { IGetRefetchPoliciesResult } from './crawler.queries';
import { updateRefetchPolicy } from './actions';

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
  field,
  className,
  children,
}: {
  rule: IGetRefetchPoliciesResult | undefined;
  field: 'offset' | 'fetchInterval';
  className?: string;
  children: ReactNode;
}) {
  const [open, setOpen] = useState(false);
  const value = field === 'offset' ? rule?.offsetLabel : rule?.fetchInterval;
  const initial = value ? intervalInput(value) : null;
  if (!rule?.fetchInterval || !initial)
    return <span className={className}>{children}</span>;

  return (
    <Popover open={open} onOpenChange={setOpen}>
      <PopoverTrigger asChild>
        <button
          type="button"
          className={className}
          title={
            field === 'offset' ? `Edit boundary: ${value}` : `Edit interval: ${value}`
          }
        >
          {children}
        </button>
      </PopoverTrigger>
      <PopoverContent align="start" className="w-80 text-sm">
        <RefetchPolicyForm
          rule={rule}
          field={field}
          initial={initial}
          onSuccess={() => setOpen(false)}
        />
      </PopoverContent>
    </Popover>
  );
}

function RefetchPolicyForm({
  rule,
  field,
  initial,
  onSuccess,
}: {
  rule: IGetRefetchPoliciesResult;
  field: 'offset' | 'fetchInterval';
  initial: IntervalInput;
  onSuccess: () => void;
}) {
  const Form = z.object({
    amount: z
      .int()
      .refine((value) => field === 'offset' || value > 0, 'Enter a positive interval.'),
    unit: z.enum(units),
  });
  const {
    control,
    handleSubmit,
    setError,
    formState: { errors },
  } = useForm({
    resolver: zodResolver(Form),
    defaultValues: initial,
  });
  const onSubmit = async (values: z.infer<typeof Form>) => {
    try {
      await updateRefetchPolicy({
        federation: rule.federation!,
        kind: rule.kind!,
        offset: rule.offsetLabel,
        fetchInterval: rule.fetchInterval!,
        field,
        ...values,
      });
      onSuccess();
    } catch (error) {
      setError('root.server', {
        message: error instanceof Error ? error.message : 'Could not save policy.',
      });
    }
  };

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit)}>
      <h4 className="pr-6 font-semibold">
        {field === 'offset' ? 'Policy boundary' : 'Fetch interval'}
      </h4>
      <FormError error={errors.root?.server?.message} />
      <div className="grid grid-cols-2 gap-2">
        <TextFieldElement
          control={control}
          name="amount"
          type="number"
          step="1"
          required
          min={field === 'fetchInterval' ? 1 : undefined}
          label={field === 'offset' ? 'Offset' : 'Every'}
        />
        <ComboboxElement
          control={control}
          name="unit"
          label="Unit"
          options={unitOptions}
          searchable={false}
        />
      </div>
      {field === 'offset' && (
        <p className="text-xs text-neutral-11">
          Offsets start at 00:00 UTC on the reference day. +1 day is the next midnight.
        </p>
      )}
      <div className="flex flex-wrap gap-4">
        <SubmitButton control={control}>Save</SubmitButton>
      </div>
    </form>
  );
}
