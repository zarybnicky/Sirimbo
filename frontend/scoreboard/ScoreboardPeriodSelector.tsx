import React from 'react';
import { PeriodPreset, periodLabels } from './periods';

type ScoreboardPeriodSelectorProps = {
  preset: PeriodPreset;
  onPresetChange: (preset: PeriodPreset) => void;
  referenceDate: string;
  onReferenceDateChange: (value: string) => void;
  customSince: string;
  onCustomSinceChange: (value: string) => void;
  customUntil: string;
  onCustomUntilChange: (value: string) => void;
  showCustomRangeWarning: boolean;
};

export function ScoreboardPeriodSelector({
  preset,
  onPresetChange,
  referenceDate,
  onReferenceDateChange,
  customSince,
  onCustomSinceChange,
  customUntil,
  onCustomUntilChange,
  showCustomRangeWarning,
}: ScoreboardPeriodSelectorProps) {
  return (
    <>
      <label className="flex flex-col gap-1 text-sm font-medium">
        Období
        <select
          className="rounded-md border border-border bg-background px-3 py-2 text-base"
          value={preset}
          onChange={(event) => onPresetChange(event.target.value as PeriodPreset)}
        >
          {Object.entries(periodLabels).map(([value, label]) => (
            <option key={value} value={value}>
              {label}
            </option>
          ))}
        </select>
      </label>

      {preset === 'custom' ? (
        <>
          <label className="flex flex-col gap-1 text-sm font-medium">
            Od
            <input
              type="date"
              className="rounded-md border border-border bg-background px-3 py-2 text-base"
              value={customSince}
              onChange={(event) => onCustomSinceChange(event.target.value)}
            />
          </label>
          <label className="flex flex-col gap-1 text-sm font-medium">
            Do
            <input
              type="date"
              className="rounded-md border border-border bg-background px-3 py-2 text-base"
              value={customUntil}
              onChange={(event) => onCustomUntilChange(event.target.value)}
            />
          </label>
          {showCustomRangeWarning ? (
            <p className="text-sm text-muted-foreground md:col-span-2 xl:col-span-4">
              Vyplňte prosím datum od i do, aby bylo možné žebříček spočítat.
            </p>
          ) : null}
        </>
      ) : (
        <label className="flex flex-col gap-1 text-sm font-medium">
          Referenční datum
          <input
            type="date"
            className="rounded-md border border-border bg-background px-3 py-2 text-base"
            value={referenceDate}
            onChange={(event) => onReferenceDateChange(event.target.value)}
          />
        </label>
      )}
    </>
  );
}
