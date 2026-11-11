'use client';

import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import {
  parseAsIsoDateTime,
  parseAsString,
  parseAsStringEnum,
  useQueryStates,
} from 'nuqs';
import {
  useForm,
  useFormState,
  useWatch,
  type Control,
  type UseFormRegister,
} from 'react-hook-form';
import { ArrowRight, Check, Clock3, TriangleAlert, PlusIcon, Copy } from 'lucide-react';
import { useMutation, useQuery } from 'urql';
import { addMonths, format } from 'date-fns';
import type { PaymentGroupItemInput, PaymentStatus } from '@/graphql';
import { PaymentGroupsDocument, SavePaymentGroupDocument } from '@/graphql/Payment';
import { cn } from '@/lib/cn';
import { EntityLink } from '@/ui/EntityLink';
import { FormError, useFormResult } from '@/ui/form';
import { compactDateFormatter, moneyFormatter, paymentStatusLabels } from '@/ui/format';
import { buttonCls } from '@/ui/style';
import { FacetRadioGroup } from '@/ui/FacetRadioGroup';
import { TextFieldElement } from '@/ui/fields/text';
import { Table, TableStatus } from '@/ui/Table';
import { SubmitButton } from '@/ui/submit';
import { Dialog, DialogContent, DialogTitle } from '@/ui/dialog';
import {
  getPeriodRange,
  periodOptions,
  PeriodPicker,
  type PeriodUnit,
} from '@/ui/PeriodPicker';

type Values = { dates: string[]; amounts: Record<string, string> };

type ColumnValues = {
  dueAt: string;
  sourceDate?: string;
  currency: string;
  amounts: Record<string, string>;
};

type Cell = {
  key: string;
  dueAt: string;
  paymentId?: string;
  amount: string;
  status: PaymentStatus;
  outside: boolean;
};

type Row = { id: string; name: string; isCurrentMember: boolean; cells: Cell[] };

type Person = {
  id: string;
  name: string;
  ranges: { since: number; until: number }[];
  payments: Record<string, { id: string; amount: string; status: PaymentStatus }>;
};

const paymentStatusIcons = {
  PAID: <Check className="size-4 text-green-11" />,
  TENTATIVE: <Clock3 className="size-4 text-neutral-11" />,
  UNPAID: <TriangleAlert className="size-4 text-warning-11" />,
};

const cellClass = 'border-b border-r border-neutral-5 px-2 py-1 text-center';
const nameCellClass = cn(cellClass, 'sticky left-0 z-10 bg-neutral-1 text-left');
const filterClass =
  'grid min-w-0 grid-cols-1 gap-x-3 gap-y-1 sm:grid-cols-[3rem_minmax(0,1fr)] disabled:opacity-50';
const buttonClass = buttonCls({
  variant: 'outline',
  size: 'sm',
  className: 'normal-case',
});
const iconButtonClass =
  'inline-flex align-middle text-neutral-11 hover:text-neutral-12 disabled:opacity-50';

export function PaymentGroups() {
  const [filters, setFilters] = useQueryStates(
    {
      groupId: parseAsString.withDefault(''),
      unit: parseAsStringEnum<PeriodUnit>(periodOptions.map((x) => x.value)).withDefault(
        '12months',
      ),
      date: parseAsIsoDateTime,
    },
    { shallow: true, scroll: false, urlKeys: { unit: 'groupUnit', date: 'groupDate' } },
  );
  const [creating, setCreating] = React.useState<ColumnValues>();
  const [now] = React.useState(Date.now);
  const period = React.useMemo(
    () => ({ unit: filters.unit, date: filters.date ?? new Date(now) }),
    [filters.unit, filters.date, now],
  );
  const [query] = useQuery({
    query: PaymentGroupsDocument,
    variables: { condition: filters.groupId ? { id: filters.groupId } : null },
  });
  const group = query.data?.selected?.[0];

  const headerRef = React.useRef<HTMLDivElement>(null);
  const scrollRef = React.useRef<HTMLDivElement>(null);

  const loaded = React.useMemo(() => {
    const range = getPeriodRange(period.unit, period.date);
    const since = range?.since.getTime() ?? -Infinity;
    const until = range?.until.getTime() ?? Infinity;
    const people = new Map<string, Person>();
    const dates = new Set<string>();

    for (const member of group?.cohort?.cohortMembershipsList ?? []) {
      if (!member.person) continue;
      const person = people.get(member.person.id) ?? {
        ...member.person,
        ranges: [],
        payments: {},
      };
      person.ranges.push({
        since: Date.parse(member.since),
        until: member.until ? Date.parse(member.until) : Infinity,
      });
      people.set(person.id, person);
    }

    for (const payment of group?.paymentsList ?? []) {
      if (
        !payment.dueAt ||
        Date.parse(payment.dueAt) < since ||
        Date.parse(payment.dueAt) >= until
      )
        continue;
      const dueAt = new Date(payment.dueAt).toISOString();
      dates.add(dueAt);

      for (const debtor of payment.paymentDebtorsList) {
        if (!debtor.person) continue;
        if (group?.cohort && !people.has(debtor.person.id)) continue;
        const person = people.get(debtor.person.id) ?? {
          ...debtor.person,
          ranges: [],
          payments: {},
        };
        person.payments[dueAt] = {
          id: payment.id,
          amount: debtor.price?.amount == null ? '' : String(Number(debtor.price.amount)),
          status: payment.status,
        };
        people.set(person.id, person);
      }
    }

    const sortedDates = [...dates].toSorted();
    const sortedPeople = [...people.values()]
      .filter(
        (x) => !group?.cohort || x.ranges.some((r) => r.since < until && r.until > since),
      )
      .toSorted((a, b) => a.name.localeCompare(b.name, 'cs'));

    const defaults: Values = { dates: sortedDates, amounts: {} };
    for (const p of sortedPeople) {
      for (const dueAt of sortedDates)
        defaults.amounts[`${p.id}_${Date.parse(dueAt)}`] =
          p.payments[dueAt]?.amount ?? '';
    }
    return { people: sortedPeople, defaults };
  }, [group, period]);

  const [, save] = useMutation(SavePaymentGroupDocument);
  const {
    control,
    register,
    handleSubmit,
    getValues,
    setValue,
    reset,
    formState: { isDirty },
  } = useForm<Values>({
    defaultValues: loaded.defaults,
  });

  const dates = useWatch({ control, name: 'dates' });
  React.useLayoutEffect(() => {
    reset(loaded.defaults, { keepDirtyValues: true });
  }, [loaded.defaults, reset]);
  const rows = React.useMemo<Row[]>(
    () =>
      loaded.people.map((person) => ({
        id: person.id,
        name: person.name,
        isCurrentMember: person.ranges.some((r) => r.since <= now && now < r.until),
        cells: dates.map((dueAt) => {
          const time = Date.parse(dueAt);
          const payment = person.payments[dueAt];
          return {
            key: `${person.id}_${time}`,
            dueAt,
            paymentId: payment?.id,
            amount: payment?.amount ?? '',
            status: payment?.status ?? (time > now ? 'TENTATIVE' : 'UNPAID'),
            outside: !person.ranges.some((r) => r.since <= time && time < r.until),
          };
        }),
      })),
    [loaded.people, dates, now],
  );
  const columns = React.useMemo(
    () =>
      dates.map((dueAt, index) => ({
        dueAt,
        label: compactDateFormatter.format(new Date(dueAt)),
        fields: rows.map((row) => `amounts.${row.cells[index]!.key}` as const),
      })),
    [dates, rows],
  );
  const initialColumn = React.useMemo(() => {
    const closest = dates.toSorted(
      (a, b) => Math.abs(Date.parse(a) - now) - Math.abs(Date.parse(b) - now),
    )[0];
    return closest ? dates.indexOf(closest) : 0;
  }, [dates, now]);

  React.useEffect(() => {
    if (!scrollRef.current || !headerRef.current) return;
    const width =
      headerRef.current.querySelector('th:nth-child(2)')?.getBoundingClientRect().width ??
      0;
    scrollRef.current.scrollLeft = initialColumn * width;
    headerRef.current.scrollLeft = scrollRef.current.scrollLeft;
  }, [initialColumn, group?.id]);

  const syncScroll = React.useCallback((event: React.UIEvent<HTMLDivElement>) => {
    if (headerRef.current) headerRef.current.scrollLeft = event.currentTarget.scrollLeft;
  }, []);

  const moveCell = React.useCallback(
    (event: React.KeyboardEvent<HTMLTableSectionElement>) => {
      if (event.key !== 'Enter' || !(event.target instanceof HTMLInputElement)) return;
      event.preventDefault();
      const cell = event.target.closest('td');
      if (!cell) return;
      const row = event.shiftKey
        ? cell.parentElement?.previousElementSibling
        : cell.parentElement?.nextElementSibling;
      const input = row?.children[cell.cellIndex]?.querySelector('input');
      input?.focus();
      input?.select();
    },
    [],
  );

  const addColumn = React.useCallback(
    (column: ColumnValues) => {
      const date = column.dueAt;
      if (dates.some((d) => format(new Date(d), 'yyyy-MM-dd') === date)) return false;
      const dueAt = new Date(`${date}T00:00:00`).toISOString();
      const values = getValues();
      const amounts = { ...values.amounts };
      for (const [personId, amount] of Object.entries(column.amounts)) {
        amounts[`${personId}_${Date.parse(dueAt)}`] = amount;
      }
      setValue('amounts', amounts, { shouldDirty: true });
      setValue('dates', [...dates, dueAt].toSorted(), { shouldDirty: true });
      return true;
    },
    [dates, getValues, setValue],
  );

  const submit = useAsyncCallback(async (values: Values) => {
    const payments: PaymentGroupItemInput[] = [];
    for (const row of rows) {
      for (const cell of row.cells) {
        const amount = values.amounts[cell.key] ?? '';
        if (amount !== '' && amount !== cell.amount)
          payments.push({ personId: row.id, dueAt: cell.dueAt, amount });
      }
    }
    const result = await save({ groupId: group!.id, payments });
    if (result.error) throw result.error;
    reset(values, { keepDirtyValues: false });
  });

  const cancel = React.useCallback(() => {
    reset(loaded.defaults, { keepDirtyValues: false });
  }, [reset, loaded.defaults]);

  const groupOptions = React.useMemo(
    () =>
      (query.data?.groups ?? []).map((g) => ({
        value: g.id,
        label: g.cohort?.name ?? g.id,
      })),
    [query.data?.groups],
  );

  const lastDate = dates.at(-1);
  const nextDate = format(
    lastDate
      ? addMonths(new Date(lastDate), 1)
      : (getPeriodRange(period.unit, period.date)?.since ?? new Date(now)),
    'yyyy-MM-dd',
  );

  const openColumn = React.useCallback(
    (sourceDate?: string) => {
      if (!group) return;
      const values = getValues();
      const amounts: Record<string, string> = {};
      for (const row of rows) {
        amounts[row.id] = sourceDate
          ? (values.amounts[`${row.id}_${Date.parse(sourceDate)}`] ?? '')
          : '';
      }
      setCreating({
        dueAt: nextDate,
        sourceDate,
        currency: group.currency ?? 'CZK',
        amounts,
      });
    },
    [getValues, rows, nextDate, group],
  );

  return (
    <div className="col-full-width content">
      <FormError error={query.error ?? submit.error} />
      <form
        className="col-full-width isolate"
        onSubmit={handleSubmit(submit.execute)}
        style={
          { '--payment-columns': `12rem repeat(${dates.length + 1}, 6rem)` } as React.CSSProperties
        }
      >
        <div className="sticky top-0 z-20 bg-neutral-1 content">
          <div className="col-content my-2 grid gap-1 lg:col-popout">
            <fieldset
              disabled={isDirty || query.fetching || submit.loading}
              className={filterClass}
            >
              <PeriodPicker
                include12Months
                includeAll
                name="payment-groups-period"
                value={period}
                onChange={({ unit, date }) => void setFilters({ unit, date })}
              />
            </fieldset>

            <div className="flex items-start gap-2">
              <fieldset
                disabled={isDirty || query.fetching || submit.loading}
                className={cn(filterClass, 'flex-1')}
              >
                <FacetRadioGroup
                  inline
                  name="payment-groups-group"
                  label="Skupina"
                  value={group?.id ?? ''}
                  onChange={(groupId) => void setFilters({ groupId })}
                  options={groupOptions}
                />
              </fieldset>
              <div className="ml-auto flex gap-2">
                <button
                  type="button"
                  disabled={!isDirty || submit.loading}
                  className={buttonClass}
                  onClick={cancel}
                >
                  Zrušit
                </button>
                <SubmitButton
                  type="submit"
                  state={submit.status}
                  disabled={!isDirty}
                  size="sm"
                  className="h-8 normal-case"
                />
              </div>
            </div>
          </div>

          <div ref={headerRef} className="col-full-width overflow-hidden">
            <table className="mx-auto block w-max text-sm">
              <thead className="block">
                <tr className="grid grid-cols-(--payment-columns)">
                  <th className={nameCellClass}>Osoba</th>
                  {columns.map((column) => (
                    <th key={column.dueAt} className={cellClass}>
                      <div className="flex items-center justify-between gap-1">
                        <button
                          type="button"
                          title="Duplikovat sloupec"
                          disabled={submit.loading}
                          className={iconButtonClass}
                          onClick={() => openColumn(column.dueAt)}
                        >
                          <Copy className="size-4" />
                        </button>
                        {column.label}
                      </div>
                    </th>
                  ))}
                  <th className={cellClass}>
                    <button
                      type="button"
                      title="Přidat sloupec"
                      disabled={!group || submit.loading}
                      className={iconButtonClass}
                      onClick={() => openColumn(lastDate)}
                    >
                      <PlusIcon className="size-4" />
                    </button>
                  </th>
                </tr>
              </thead>
            </table>
          </div>
        </div>

        <div ref={scrollRef} className="overflow-x-auto" onScroll={syncScroll}>
          <table className="mx-auto block w-max text-sm tabular-nums">
            <tbody className="block" onKeyDown={moveCell}>
              <TableStatus
                colSpan={columns.length + 2}
                busy={query.fetching && !group}
                error={query.error}
              />
              {rows.map((person) => (
                <PaymentRow
                  key={person.id}
                  person={person}
                  register={register}
                  control={control}
                  disabled={submit.loading}
                />
              ))}
            </tbody>
            <tfoot className="block font-medium">
              <tr className="grid grid-cols-(--payment-columns)">
                <th className={nameCellClass}>Celkem {group?.currency}</th>
                {columns.map((c) => (
                  <PaymentTotal key={c.dueAt} fields={c.fields} control={control} />
                ))}
                <td className={cellClass} />
              </tr>
            </tfoot>
          </table>
        </div>
      </form>
      <Dialog
        open={!!creating}
        onOpenChange={(open) => {
          if (!open) setTimeout(() => setCreating(undefined));
        }}
      >
        <DialogContent>
          {creating && <ColumnForm initialValues={creating} onAdd={addColumn} />}
        </DialogContent>
      </Dialog>
    </div>
  );
}

const PaymentRow = React.memo(function PaymentRow({
  person,
  register,
  control,
  disabled,
}: {
  person: Row;
  register: UseFormRegister<Values>;
  control: Control<Values>;
  disabled: boolean;
}) {
  const fields = React.useMemo(
    () => person.cells.map((cell) => `amounts.${cell.key}` as const),
    [person.cells],
  );
  const { dirtyFields } = useFormState({ control, name: fields, exact: true });
  return (
    <tr className="grid grid-cols-(--payment-columns)">
      <th scope="row" className={cn(nameCellClass, 'font-normal')}>
        <EntityLink type="person" entity={person} className="block truncate" />
      </th>
      {person.cells.map((cell) => {
        const dirty = dirtyFields.amounts?.[cell.key];
        const hasAmount = cell.amount !== '' || dirty;
        return (
          <td
            key={cell.key}
            className={cn(
              cellClass,
              'flex items-center gap-1',
              cell.outside && 'bg-neutral-3',
              dirty && 'bg-accent-4',
            )}
          >
            <span
              className="w-6 shrink-0"
              title={hasAmount ? paymentStatusLabels[cell.status] : undefined}
            >
              {hasAmount &&
                (cell.paymentId ? (
                  <a
                    href={`/platby/${cell.paymentId}`}
                    className="mx-auto block w-4 border-b border-dotted border-accent-9 hover:opacity-70"
                  >
                    {paymentStatusIcons[cell.status]}
                  </a>
                ) : (
                  <span className="flex items-center justify-center text-xs opacity-30">
                    {paymentStatusIcons[cell.status]}
                  </span>
                ))}
            </span>
            <input
              {...register(`amounts.${cell.key}`)}
              defaultValue={cell.amount}
              type="number"
              step="0.01"
              disabled={disabled}
              required={cell.amount !== ''}
              className="w-full min-w-0 rounded-none border-0 bg-transparent px-0 py-0.5 text-right text-sm tabular-nums focus:ring-2 focus:ring-accent-8"
            />
          </td>
        );
      })}
      <td className={cn(cellClass, !person.isCurrentMember && 'bg-neutral-3')} />
    </tr>
  );
});

const PaymentTotal = React.memo(function PaymentTotal({
  fields,
  control,
}: {
  fields: `amounts.${string}`[];
  control: Control<Values>;
}) {
  const amounts = useWatch({ control, name: fields, exact: true });
  return (
    <td className={cellClass}>
      {amounts.reduce((sum, amount) => sum + Number(amount || 0), 0).toLocaleString('cs')}
    </td>
  );
});

function ColumnForm({
  initialValues,
  onAdd,
}: {
  initialValues: ColumnValues;
  onAdd: (values: ColumnValues) => boolean;
}) {
  const { onSuccess } = useFormResult();

  const groups = React.useMemo(() => {
    const byAmount = new Map<number | null, string[]>();
    for (const [personId, value] of Object.entries(initialValues.amounts)) {
      const amount = value === '' ? null : Number(value);
      const people = byAmount.get(amount) ?? [];
      people.push(personId);
      byAmount.set(amount, people);
    }
    return [...byAmount].toSorted(([a], [b]) => (b ?? -Infinity) - (a ?? -Infinity));
  }, [initialValues.amounts]);

  const maximum = groups[0]?.[0] ?? undefined;
  const {
    control,
    handleSubmit,
    setError,
    resetField,
    getFieldState,
    formState: { dirtyFields },
  } = useForm({
    defaultValues: {
      dueAt: initialValues.dueAt,
      rates: groups.map(([amount]) => amount ?? maximum),
    },
  });

  const firstAmount = useWatch({ control, name: 'rates.0' });
  React.useLayoutEffect(() => {
    if (firstAmount === undefined) return;
    for (let index = 1; index < groups.length; index++) {
      const name = `rates.${index}` as const;
      if (getFieldState(name).isDirty) continue;
      let amount = groups[index]![0];
      if (amount === null) amount = firstAmount;
      else if (maximum)
        amount = Math.round(((amount * firstAmount) / maximum) * 100) / 100;
      resetField(name, { defaultValue: amount });
    }
  }, [firstAmount, maximum, groups, getFieldState, resetField]);

  const onSubmit = handleSubmit((values) => {
    const amounts: Record<string, string> = {};
    for (const [index, [, people]] of groups.entries()) {
      for (const personId of people) amounts[personId] = String(values.rates[index]);
    }
    if (!onAdd({ ...initialValues, dueAt: values.dueAt, amounts })) {
      setError('dueAt', { message: 'Sloupec pro toto datum už existuje.' });
      return;
    }
    onSuccess();
  });

  return (
    <form className="grid gap-3" onSubmit={onSubmit}>
      <DialogTitle>
        {initialValues.sourceDate ? 'Duplikovat sloupec' : 'Přidat sloupec'}
      </DialogTitle>
      <TextFieldElement
        control={control}
        name="dueAt"
        label="Splatnost"
        type="date"
        required
      />
      <Table
        columns={[
          'Osob',
          'Částka',
          { label: null, colSpan: 1 },
          `Nová částka (${initialValues.currency})`,
        ]}
      >
        {groups.map(([amount, people], index) => (
          <tr key={amount ?? 'missing'}>
            <td>{people.length}x</td>
            <td>
              {amount === null
                ? 'Bez platby'
                : moneyFormatter.format({ amount, currency: initialValues.currency })}
            </td>
            <td className="w-4">
              <ArrowRight className="size-4 text-neutral-10" strokeWidth={1} />
            </td>
            <td>
              <TextFieldElement
                control={control}
                name={`rates.${index}`}
                type="number"
                min={0}
                step="0.01"
                required
                inputClassName={dirtyFields.rates?.[index] ? 'bg-accent-3' : undefined}
              />
            </td>
          </tr>
        ))}
      </Table>
      <p className="text-xs text-neutral-11">
        Jde o výchozí částky. Platby se vytvoří až tlačítkem Uložit v hlavní tabulce.
      </p>
      <SubmitButton control={control}>
        {initialValues.sourceDate ? 'Duplikovat' : 'Přidat'}
      </SubmitButton>
    </form>
  );
}
