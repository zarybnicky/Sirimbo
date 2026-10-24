import { DotSeparated } from '@/ui/DotSeparated';
/* eslint-disable react-hooks/refs -- Keep lookup history and automatic-selection metadata alongside the RHF value. */
import { FederationPersonLookupDocument } from '@/graphql/Federation';
import { formatAgeGroup } from '@/ui/csts';
import { FederationLink } from '@/ui/federation-links';
import { TextField, type TextFieldElementProps } from '@/ui/fields/text';
import React from 'react';
import { type FieldValues, type Path, useController, useWatch } from 'react-hook-form';
import { useQuery } from 'urql';

export function FederationIdFieldElement<T extends FieldValues>({
  control,
  name,
  federation,
  placeholder = '10000000',
  onChange,
  onBlur,
  ...props
}: TextFieldElementProps<T> & { federation: string }) {
  const { field, fieldState } = useController({ control, name });
  const prefix = name.slice(0, name.lastIndexOf('.') + 1);
  const [first, last, sex, dob] = useWatch({
    control,
    name: [
      `${prefix}firstName`,
      `${prefix}lastName`,
      `${prefix}gender`,
      `${prefix}birthDate`,
    ] as Path<T>[],
  });
  const firstName = typeof first === 'string' ? first.trim() : '';
  const lastName = typeof last === 'string' ? last.trim() : '';
  const gender = sex === 'MAN' || sex === 'WOMAN' ? sex : null;
  const birthDate =
    typeof dob === 'string' && /^\d{4}-\d{2}-\d{2}$/.test(dob) ? dob : null;
  const id = Number(field.value);
  const externalId = Number.isSafeInteger(id) && id > 0 ? String(id) : '';
  const selection = React.useRef({ automaticId: null as number | null, manual: false });
  const byId = !!externalId && id !== selection.current.automaticId;
  const variables = byId
    ? { federation, externalId, byId: true }
    : {
        federation,
        externalId: '0',
        byId: false,
        firstName,
        lastName,
        gender,
        birthDate,
      };
  const input = JSON.stringify(variables);
  const [settledInput, setSettledInput] = React.useState(input);

  React.useEffect(() => {
    const timeout = setTimeout(() => setSettledInput(input), 300);
    return () => clearTimeout(timeout);
  }, [input]);

  const ready = input === settledInput;
  const [query] = useQuery({
    query: FederationPersonLookupDocument,
    pause: !ready || (!byId && (!firstName || !lastName)),
    variables,
  });
  const resolved = ready && !query.fetching && !query.stale && !query.error;
  const currentData = resolved ? query.data : undefined;
  const previousData = React.useRef(currentData);
  React.useEffect(() => {
    if (currentData) previousData.current = currentData;
  }, [currentData]);
  const data = currentData ?? previousData.current;
  const candidate = data?.candidates?.[0];
  const person = data?.person;
  React.useEffect(() => {
    if (selection.current.manual || byId || !resolved) return;
    const nextId =
      firstName && lastName && candidate?.isStrongMatch ? candidate.id : undefined;
    const mayUpdate =
      field.value == null || Number(field.value) === selection.current.automaticId;
    if (mayUpdate && field.value !== nextId) {
      selection.current.automaticId = nextId ?? null;
      field.onChange(nextId);
    }
  }, [byId, resolved, firstName, lastName, candidate, field]);

  let helperText = props.helperText;
  if (ready && query.error) {
    helperText = 'Vyhledávání se nezdařilo';
  } else if (externalId && (person || candidate?.id === id)) {
    helperText = (
      <FederationLink
        entity="people"
        federation={federation}
        externalId={person?.externalId ?? externalId}
      >
        {person
          ? person.canonicalName ||
            [person.firstName, person.lastName].filter(Boolean).join(' ') ||
            person.externalId
          : candidate?.name}
      </FederationLink>
    );
  } else if (byId && resolved && data) {
    helperText = 'Nenalezeno';
  } else if (candidate) {
    helperText = (
      <button
        type="button"
        className="grid w-full gap-0.5 rounded px-2 py-1 text-left hover:bg-accent-3 focus-visible:outline-accent-8"
        onClick={() => {
          selection.current = { manual: true, automaticId: null };
          field.onChange(candidate.id);
        }}
      >
        <span className="text-neutral-12">Je to {candidate.name}?</span>
        <DotSeparated className="text-xs text-neutral-11">
          {[
            candidate.birthYear,
            formatAgeGroup(candidate.ageGroup),
            `${federation === 'csts' ? 'IDT' : 'MIN'} ${candidate.id}`,
          ]}
        </DotSeparated>
      </button>
    );
  }

  return (
    <TextField
      {...props}
      name={name}
      value={field.value ?? ''}
      error={fieldState.error}
      placeholder={placeholder}
      type="number"
      helperText={helperText}
      onChange={(event) => {
        onChange?.(event);
        selection.current = { manual: true, automaticId: null };
        const value = event.currentTarget.valueAsNumber;
        field.onChange(Number.isNaN(value) ? undefined : value);
      }}
      onBlur={(event) => {
        onBlur?.(event);
        field.onBlur();
      }}
    />
  );
}
