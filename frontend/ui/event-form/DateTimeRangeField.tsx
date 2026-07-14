import { cn } from '@/lib/cn';
import { TextField } from '@/ui/fields/text';
import {
  addHours,
  addMilliseconds,
  differenceInMilliseconds,
  format,
  isSameDay,
  isValid,
  parse,
} from 'date-fns';
import { type Control, useController, useWatch } from 'react-hook-form';
import { EventForm } from '@/ui/event-form/types';
import { z } from 'zod';

function parseLocalDateTime(date: string, time: string) {
  const value = parse(`${date} ${time}`, 'yyyy-MM-dd HH:mm', new Date());
  return isValid(value) ? value : null;
}

export function DateTimeRangeField({
  control,
  nameSince,
  nameUntil,
  className,
}: {
  control: Control<z.input<typeof EventForm>, unknown, z.infer<typeof EventForm>>;
  nameSince: `instances.${number}.since`;
  nameUntil: `instances.${number}.until`;
  className?: string;
}) {
  const since = useController({ control, name: nameSince });
  const until = useController({ control, name: nameUntil });
  const type = useWatch({ control, name: 'type' });

  const sinceDate = since.field.value
    ? new Date(since.field.value as string)
    : new Date();
  const untilDate = until.field.value
    ? new Date(until.field.value as string)
    : addHours(sinceDate, 1);
  const startDate = format(sinceDate, 'yyyy-MM-dd');
  const startTime = format(sinceDate, 'HH:mm');
  const endDate = format(untilDate, 'yyyy-MM-dd');
  const endTime = format(untilDate, 'HH:mm');
  const showEndDate = type === 'CAMP' || !isSameDay(sinceDate, untilDate);

  const setSince = (date: string, time: string) => {
    const nextSince = parseLocalDateTime(date, time);
    if (!nextSince) return;

    const duration = Math.max(0, differenceInMilliseconds(untilDate, sinceDate));
    since.field.onChange(nextSince.toISOString());
    until.field.onChange(addMilliseconds(nextSince, duration).toISOString());
  };

  const setUntil = (date: string, time: string) => {
    const nextUntil = parseLocalDateTime(date, time);
    if (nextUntil) until.field.onChange(nextUntil.toISOString());
  };

  return (
    <div
      className={cn(
        'flex min-w-0 flex-1 gap-2',
        type === 'CAMP' ? 'flex-col' : 'flex-wrap items-baseline',
        className,
      )}
    >
      <div className="flex flex-wrap items-baseline gap-2">
        <TextField
          type="date"
          value={startDate}
          aria-label={type === 'CAMP' ? 'Začátek (datum)' : 'Datum'}
          onChange={(event) => setSince(event.target.value, startTime)}
        />
        <TextField
          type="time"
          step={60}
          value={startTime}
          required
          aria-label="Začátek"
          onChange={(event) => setSince(startDate, event.target.value)}
          error={
            untilDate < sinceDate
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
            onChange={(event) => setUntil(startDate, event.target.value)}
          />
        )}
      </div>

      {showEndDate && (
        <div className="flex flex-wrap items-baseline gap-2">
          <TextField
            type="date"
            value={endDate}
            required={type === 'CAMP'}
            aria-label="Konec (datum)"
            onChange={(event) => setUntil(event.target.value, endTime)}
          />
          <TextField
            type="time"
            step={60}
            value={endTime}
            required
            aria-label="Konec"
            onChange={(event) => setUntil(endDate, event.target.value)}
          />
        </div>
      )}
    </div>
  );
}
