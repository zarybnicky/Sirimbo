import { cn } from '@/ui/cn';
import { EventForm } from '@/ui/event-form/types';
import { buttonCls } from '@/ui/style';
import { add } from 'date-arithmetic';
import { ChevronRightIcon, CircleAlertIcon, Plus, X } from 'lucide-react';
import React from 'react';
import {
  type Control,
  FieldValues,
  Path,
  useController,
  useFieldArray,
  useWatch,
} from 'react-hook-form';
import * as Collapsible from '@radix-ui/react-collapsible';
import { InstanceTrainerListElement } from './InstanceTrainerListField';
import { z } from 'zod';
import { TextField } from '@/ui/fields/text';

export function InstanceListElement({
  name,
  control,
}: {
  control: Control<z.input<typeof EventForm>, unknown, z.infer<typeof EventForm>>;
  name: 'instances';
}) {
  const { fields, append, remove, update } = useFieldArray({ name, control });
  const instances = useWatch({ control, name: 'instances' });
  const type = useWatch({ control, name: 'type' }) ?? 'LESSON';
  const isCamp = type === 'CAMP';

  const addInstancePlusWeek = React.useCallback(() => {
    const { since, until } = (instances || []).findLast((x) => !!x.since) ?? {};
    if (!since || !until) {
      append({
        since: new Date().toISOString(),
        until: add(new Date(), 45, 'minutes').toISOString(),
        isCancelled: false,
        trainers: [],
      });
    } else {
      append({
        since: add(new Date(since), 1, 'week').toISOString(),
        until: add(new Date(until), 1, 'week').toISOString(),
        isCancelled: false,
        trainers: [],
      });
    }
  }, [append, instances]);

  const addInstancePlus2Weeks = React.useCallback(() => {
    const { since, until } = (instances || []).findLast((x) => !!x.since) ?? {};
    if (!since || !until) {
      append({
        since: new Date().toISOString(),
        until: add(new Date(), 45, 'minutes').toISOString(),
        isCancelled: false,
        trainers: [],
      });
    } else {
      append({
        since: add(new Date(since), 2, 'week').toISOString(),
        until: add(new Date(until), 2, 'week').toISOString(),
        isCancelled: false,
        trainers: [],
      });
    }
  }, [append, instances]);

  return (
    <>
      <div className="flex flex-wrap items-baseline justify-between gap-2 pt-1">
        <div>
          <b>Termíny</b>
        </div>

        <div className="flex flex-wrap gap-2">
          <button
            type="button"
            className={buttonCls({ size: 'xs', variant: 'outline' })}
            onClick={addInstancePlusWeek}
          >
            <Plus /> 1&nbsp;týden
          </button>

          <button
            type="button"
            className={buttonCls({ size: 'xs', variant: 'outline' })}
            onClick={addInstancePlus2Weeks}
          >
            <Plus /> 2&nbsp;týdny
          </button>
        </div>
      </div>

      {fields.map((instance, index) => {
        if (!instance.since) return <React.Fragment key={instance.id} />;
        return (
          <Collapsible.Root asChild key={instance.id || index}>
            <div className="flex flex-col gap-2">
              <div className="flex flex-wrap items-start gap-2">
                <div className="relative pt-2">
                  {instance.trainers && instance.trainers.length > 0 && (
                    <div className="absolute -left-4 top-3 -mt-0.5">
                      <CircleAlertIcon className="size-4" />
                    </div>
                  )}
                  <Collapsible.Trigger asChild>
                    <button className="relative data-[state=open]:rotate-90">
                      <ChevronRightIcon />
                    </button>
                  </Collapsible.Trigger>
                </div>

                <DateTimeRangeController
                  control={control}
                  nameSince={`instances.${index}.since`}
                  nameUntil={`instances.${index}.until`}
                  isCamp={isCamp}
                />

                <button
                  type="button"
                  className={cn(
                    buttonCls({ size: 'sm', variant: 'outline' }),
                    'self-start',
                  )}
                  disabled={fields.filter((x) => !!x.since).length <= 1}
                  onClick={() =>
                    instance.itemId
                      ? update(index, { ...instance, since: null, until: null })
                      : remove(index)
                  }
                >
                  <X />
                </button>
              </div>

              <Collapsible.Content
                asChild
                forceMount
                className="[&[data-state=closed]>div]:hidden"
              >
                <div className="ml-4 w-full items-baseline gap-2">
                  <InstanceTrainerListElement control={control} index={index} />
                </div>
              </Collapsible.Content>
            </div>
          </Collapsible.Root>
        );
      })}
    </>
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

function DateTimeRangeController<T extends FieldValues>({
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
