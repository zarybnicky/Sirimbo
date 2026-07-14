import React from 'react';
import { TextField } from '@/ui/fields/text';
import { SelectField, type SelectOption } from '@/ui/fields/select';
import { add } from 'date-arithmetic';

export type PeriodPreset = 'schoolyear' | 'semester' | 'quarter' | 'month' | 'custom';

type PeriodRange = {
  since: string | null;
  until: string | null;
  displaySince: Date | null;
  displayUntil: Date | null;
};

type PeriodSelectorProps = {
  preset: PeriodPreset;
  onPresetChange: (preset: PeriodPreset) => void;
  date: Date;
  onDateChange: (value: Date) => void;
  since: Date | null;
  onSinceChange: (value: Date | null) => void;
  until: Date | null;
  onUntilChange: (value: Date | null) => void;
};

const periodOptions = Object.entries({
  schoolyear: 'Školní rok',
  semester: 'Pololetí',
  quarter: 'Čtvrtletí',
  month: 'Měsíc',
  custom: 'Vlastní interval',
}).map(([value, label]) => ({
  value: value as PeriodPreset,
  label,
})) satisfies SelectOption<PeriodPreset>[];

export function PeriodSelector({
  preset,
  onPresetChange,
  date,
  onDateChange,
  since,
  onSinceChange,
  until,
  onUntilChange,
}: PeriodSelectorProps) {
  return (
    <>
      <SelectField
        label="Období"
        value={preset}
        onChange={onPresetChange}
        options={periodOptions}
      />

      {preset === 'custom' ? (
        <>
          <TextField
            label="Od"
            name="scoreboard-since"
            type="date"
            value={since?.toISOString()?.slice(0, 10)}
            onChange={(event) =>
              onSinceChange(
                event.currentTarget.value ? new Date(event.currentTarget.value) : null,
              )
            }
          />
          <TextField
            label="Do"
            name="scoreboard-until"
            type="date"
            value={until?.toISOString()?.slice(0, 10)}
            onChange={(event) =>
              onUntilChange(
                event.currentTarget.value ? new Date(event.currentTarget.value) : null,
              )
            }
          />
        </>
      ) : (
        <TextField
          label="K datu"
          name="scoreboard-date"
          type="date"
          value={date.toISOString().slice(0, 10)}
          onChange={(event) =>
            onDateChange(
              event.currentTarget.value
                ? new Date(event.currentTarget.value)
                : new Date(),
            )
          }
        />
      )}
    </>
  );
}

export const computeRange = (
  preset: PeriodPreset,
  date: Date,
  customSince: Date | null,
  customUntil: Date | null,
): PeriodRange => {
  if (preset === 'custom') {
    if (!customSince || !customUntil) {
      return {
        since: null,
        until: null,
        displaySince: customSince,
        displayUntil: customUntil,
      };
    }

    if (!customSince || !customUntil) {
      return { since: null, until: null, displaySince: null, displayUntil: null };
    }

    const untilExclusive = add(customUntil, 1, 'day');
    return {
      since: customSince.toISOString().slice(0, 10),
      until: untilExclusive.toISOString().slice(0, 10),
      displaySince: customSince,
      displayUntil: customUntil,
    };
  }

  const schoolYear =
    date.getUTCMonth() >= 8 ? date.getUTCFullYear() : date.getUTCFullYear() - 1;
  const schoolYearStart = new Date(Date.UTC(schoolYear, 8, 1));

  if (preset === 'schoolyear') {
    const untilDate = add(schoolYearStart, 12, 'month');
    return {
      since: schoolYearStart.toISOString().slice(0, 10),
      until: untilDate.toISOString().slice(0, 10),
      displaySince: schoolYearStart,
      displayUntil: add(untilDate, -1, 'month'),
    };
  }

  const monthsDiff =
    (date.getUTCFullYear() - schoolYearStart.getUTCFullYear()) * 12 +
    (date.getUTCMonth() - schoolYearStart.getUTCMonth());

  if (preset === 'semester') {
    const isSecondSemester = monthsDiff >= 5;
    const sinceDate = isSecondSemester
      ? add(schoolYearStart, 5, 'month')
      : schoolYearStart;
    const untilDate = isSecondSemester
      ? add(schoolYearStart, 12, 'month')
      : add(schoolYearStart, 5, 'month');
    return {
      since: sinceDate.toISOString().slice(0, 10),
      until: untilDate.toISOString().slice(0, 10),
      displaySince: sinceDate,
      displayUntil: add(untilDate, -1, 'day'),
    };
  }

  if (preset === 'month') {
    const monthIndex = Math.max(0, Math.min(11, monthsDiff));
    const sinceDate = add(schoolYearStart, monthIndex, 'month');
    const untilDate = add(sinceDate, 1, 'month');
    return {
      since: sinceDate.toISOString().slice(0, 10),
      until: untilDate.toISOString().slice(0, 10),
      displaySince: sinceDate,
      displayUntil: add(untilDate, -1, 'day'),
    };
  }

  if (preset === 'quarter') {
    const quarterIndex = Math.max(0, Math.min(3, Math.floor(monthsDiff / 3)));
    const sinceDate = add(schoolYearStart, quarterIndex * 3, 'month');
    const untilDate = add(sinceDate, 3, 'month');
    return {
      since: sinceDate.toISOString().slice(0, 10),
      until: untilDate.toISOString().slice(0, 10),
      displaySince: sinceDate,
      displayUntil: add(untilDate, -1, 'day'),
    };
  }

  return { since: null, until: null, displaySince: null, displayUntil: null };
};
