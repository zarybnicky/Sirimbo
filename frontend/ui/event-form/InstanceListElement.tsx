import { cn } from '@/lib/cn';
import { EventForm } from '@/ui/event-form/types';
import { buttonCls } from '@/ui/style';
import { add } from 'date-arithmetic';
import { Plus, X } from 'lucide-react';
import React from 'react';
import {
  type Control,
  FieldValues,
  Path,
  useController,
  useFieldArray,
  useWatch,
} from 'react-hook-form';
import { TextField } from '@/ui/fields/text';
import { z } from 'zod';

export function InstanceListElement({
  control,
}: {
  control: Control<z.input<typeof EventForm>, unknown, z.infer<typeof EventForm>>;
}) {
  const { fields, append, remove } = useFieldArray({ name: 'instances', control });
  const instances = useWatch({ control, name: 'instances' });
  const type = useWatch({ control, name: 'type' }) ?? 'LESSON';

  const addInstance = React.useCallback(
    (weeks: 1 | 2) => {
      const previous = (instances || []).findLast(
        (instance) => instance.since && instance.until,
      );
      const since = previous?.since ? new Date(previous.since) : new Date();
      const until = previous?.until
        ? new Date(previous.until)
        : add(since, 45, 'minutes');
      append({
        itemId: null,
        since: add(since, weeks, 'week').toISOString(),
        until: add(until, weeks, 'week').toISOString(),
        isCancelled: false,
        trainers: [],
      });
    },
    [append, instances],
  );

  return (
    <div className="space-y-2">
      <div className="flex flex-wrap items-baseline justify-between gap-2 pt-1">
        <b>Termíny</b>
        <div className="flex flex-wrap gap-2">
          <button
            type="button"
            className={buttonCls({ size: 'xs', variant: 'outline' })}
            onClick={() => addInstance(1)}
          >
            <Plus /> 1&nbsp;týden
          </button>
          <button
            type="button"
            className={buttonCls({ size: 'xs', variant: 'outline' })}
            onClick={() => addInstance(2)}
          >
            <Plus /> 2&nbsp;týdny
          </button>
        </div>
      </div>

      {fields.map((instance, index) => (
        <div className="flex flex-wrap items-start gap-2" key={instance.id}>
          <DateTimeRangeController
            control={control}
            nameSince={`instances.${index}.since`}
            nameUntil={`instances.${index}.until`}
            isCamp={type === 'CAMP'}
          />
          {!instance.itemId && fields.length > 1 ? (
            <button
              type="button"
              className={buttonCls({ size: 'sm', variant: 'outline' })}
              aria-label={`Odstranit termín ${index + 1}`}
              onClick={() => remove(index)}
            >
              <X />
            </button>
          ) : null}
        </div>
      ))}
    </div>
  );
}

function pad2(n: number) {
  return String(n).padStart(2, '0');
}
function toDateValue(d: Date) {
  return `${d.getFullYear()}-${pad2(d.getMonth() + 1)}-${pad2(d.getDate())}`;
}
function toTimeValue(d: Date) {
  return `${pad2(d.getHours())}:${pad2(d.getMinutes())}`;
}
function parseTime(t: string) {
  const [hh = '0', mm = '0', ss = '0'] = t.split(':');
  return { h: Number(hh), m: Number(mm), s: Number(ss) };
}
function fromLocalParts(date: string, time: string) {
  const [y, mo, da] = date.split('-').map(Number);
  const { h, m, s } = parseTime(time);
  return new Date(y!, (mo ?? 1) - 1, da ?? 1, h ?? 0, m ?? 0, s ?? 0, 0);
}
function isSameDay(a: Date, b: Date) {
  return (
    a.getFullYear() === b.getFullYear() &&
    a.getMonth() === b.getMonth() &&
    a.getDate() === b.getDate()
  );
}

export function DateTimeRangeController<T extends FieldValues>({
  control,
  nameSince,
  nameUntil,
  isCamp,
  className,
}: {
  control: Control<T>;
  nameSince: Path<T>;
  nameUntil: Path<T>;
  isCamp: boolean;
  className?: string;
}) {
  const since = useController({ control, name: nameSince });
  const until = useController({ control, name: nameUntil });

  const sinceDate = React.useMemo(() => {
    const v = since.field.value as string | null | undefined;
    return v ? new Date(v) : new Date();
  }, [since.field.value]);

  const untilDate = React.useMemo(() => {
    const v = until.field.value as string | null | undefined;
    return v ? new Date(v) : new Date(sinceDate.getTime() + 60 * 60 * 1000);
  }, [until.field.value, sinceDate]);

  const spansDays = !isSameDay(sinceDate, untilDate);
  const showEndDate = isCamp || spansDays;

  const startDate = toDateValue(sinceDate);
  const startTime = toTimeValue(sinceDate);
  const endDate = toDateValue(untilDate);
  const endTime = toTimeValue(untilDate);

  const invalidRange = untilDate.getTime() < sinceDate.getTime();

  // Editing start shifts end by the same duration
  const setSince = React.useCallback(
    (nextSince: Date) => {
      const dur = Math.max(0, untilDate.getTime() - sinceDate.getTime());
      const nextUntil = new Date(nextSince.getTime() + dur);
      since.field.onChange(nextSince.toISOString());
      until.field.onChange(nextUntil.toISOString());
    },
    [since.field, until.field, sinceDate, untilDate],
  );

  const setUntil = React.useCallback(
    (nextUntil: Date) => {
      until.field.onChange(nextUntil.toISOString());
    },
    [until.field],
  );

  return (
    <div
      className={cn(
        'flex min-w-0 flex-1 gap-2',
        isCamp ? 'flex-col' : 'flex-wrap items-baseline',
        className,
      )}
    >
      <div className="flex flex-wrap items-baseline gap-2">
        <TextField
          type="date"
          value={startDate}
          aria-label={isCamp ? 'Začátek (datum)' : 'Datum'}
          onChange={(e) => setSince(fromLocalParts(e.target.value, startTime))}
        />

        <TextField
          type="time"
          step={60}
          value={startTime}
          required
          aria-label="Začátek"
          onChange={(e) => setSince(fromLocalParts(startDate, e.target.value))}
          error={
            invalidRange
              ? { type: '', message: 'Konec události je dřiv než začátek' }
              : undefined
          }
        />

        {!showEndDate && (
          <TextField
            type="time"
            step={60}
            value={endTime}
            required
            aria-label="Konec"
            onChange={(e) => setUntil(fromLocalParts(startDate, e.target.value))}
          />
        )}
      </div>

      {(isCamp || showEndDate) && (
        <div className="flex flex-wrap items-baseline gap-2">
          <TextField
            type="date"
            value={endDate}
            required={isCamp}
            aria-label="Konec (datum)"
            onChange={(e) => setUntil(fromLocalParts(e.target.value, endTime))}
          />
          <TextField
            type="time"
            step={60}
            value={endTime}
            required
            aria-label="Konec"
            onChange={(e) => setUntil(fromLocalParts(endDate, e.target.value))}
          />
        </div>
      )}
    </div>
  );
}
