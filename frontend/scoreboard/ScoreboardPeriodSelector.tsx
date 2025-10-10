import React from 'react';
import { Combobox } from '@/ui/fields/Combobox';
import { TextField } from '@/ui/fields/text';
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
  const periodOptions = React.useMemo(
    () =>
      Object.entries(periodLabels).map(([value, label]) => ({
        id: value,
        label,
      })),
    [],
  );

  const handlePresetChange = React.useCallback<
    React.Dispatch<React.SetStateAction<string | null | undefined>>
  >(
    (value) => {
      const nextValue = typeof value === 'function' ? value(preset) : value;

      if (typeof nextValue === 'string') {
        onPresetChange(nextValue as PeriodPreset);
      }
    },
    [onPresetChange, preset],
  );

  return (
    <>
      <Combobox
        label="Období"
        value={preset}
        onChange={handlePresetChange}
        options={periodOptions}
        placeholder="Vyberte období"
      />

      {preset === 'custom' ? (
        <>
          <TextField
            className="space-y-1"
            label="Od"
            name="scoreboard-custom-since"
            type="date"
            value={customSince}
            onChange={(event) => onCustomSinceChange(event.currentTarget.value)}
          />
          <TextField
            className="space-y-1"
            label="Do"
            name="scoreboard-custom-until"
            type="date"
            value={customUntil}
            onChange={(event) => onCustomUntilChange(event.currentTarget.value)}
          />
          {showCustomRangeWarning ? (
            <p className="text-sm text-neutral-10 md:col-span-2 xl:col-span-4">
              Vyplňte prosím datum od i do, aby bylo možné žebříček spočítat.
            </p>
          ) : null}
        </>
      ) : (
        <TextField
          className="space-y-1"
          label="Referenční datum"
          name="scoreboard-reference-date"
          type="date"
          value={referenceDate}
          onChange={(event) => onReferenceDateChange(event.currentTarget.value)}
        />
      )}
    </>
  );
}
