import { eventTypes } from '@/calendar/state';
import type { EventSavedSearchInput } from '@/graphql';
import {
  CreateEventSavedSearchDocument,
  EventFormOptionsDocument,
  type EventSavedSearchFragment,
  UpdateEventSavedSearchDocument,
} from '@/graphql/Event';
import { isTruthy } from '@/lib/truthyFilter';
import { FacetFilter } from '@/ui/FacetFilter';
import { CheckboxElement } from '@/ui/fields/checkbox';
import { ComboboxElement } from '@/ui/fields/Combobox';
import { TextField, TextFieldElement } from '@/ui/fields/text';
import { FieldHelper, FormError, useFormResult } from '@/ui/form';
import { formatEventType, fullDateFormatter } from '@/ui/format';
import { buttonCls } from '@/ui/style';
import { SubmitButton } from '@/ui/submit';
import { zodResolver } from '@hookform/resolvers/zod';
import { Copy, Download } from 'lucide-react';
import { Controller, useForm } from 'react-hook-form';
import { toast } from 'react-toastify';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';

const optionalIds = z
  .array(z.string())
  .nullish()
  .transform((ids) => (ids?.length ? ids : null));

const Form = z.object({
  name: z.string().trim().min(1, 'Vyplňte název'),
  crossTenant: z.boolean().prefault(false),
  condition: z
    .object({
      // Null includes all event types; an empty list excludes club events.
      types: z.array(z.enum(eventTypes)).nullable().prefault(null),
      trainerIds: optionalIds,
      locationIds: optionalIds,
      participantIds: optionalIds,
      parentId: z.string().nullable().prefault(null),
      cohortId: z.string().nullable().prefault(null),
      onlyMine: z.boolean().nullable().prefault(false),
      relevantOnly: z.boolean().nullable().prefault(false),
      // An unchecked filter means no restriction, rather than unavailable events.
      canRegister: z
        .boolean()
        .nullish()
        .transform((value) => value || null),
      withCompetitions: z.boolean().nullable().prefault(false),
    })
    .prefault({}),
}) satisfies z.ZodType<EventSavedSearchInput>;

export function EventSavedSearchForm({ savedSearch }: { savedSearch?: EventSavedSearchFragment }) {
  const { onSuccess } = useFormResult();
  const [{ data, error }] = useQuery({
    query: EventFormOptionsDocument,
    variables: { withParents: true, withPeople: true, peopleCondition: null },
  });
  const [createResult, create] = useMutation(CreateEventSavedSearchDocument);
  const [updateResult, update] = useMutation(UpdateEventSavedSearchDocument);
  const condition = savedSearch?.condition;
  const { control, handleSubmit } = useForm({
    resolver: zodResolver(Form),
    defaultValues: {
      name: savedSearch?.name ?? '',
      crossTenant: savedSearch?.crossTenant ?? false,
      condition: {
        ...condition,
        types: condition?.types?.filter(isTruthy) ?? null,
        trainerIds: condition?.trainerIds?.filter(isTruthy) ?? null,
        locationIds: condition?.locationIds?.filter(isTruthy) ?? null,
        participantIds: condition?.participantIds?.filter(isTruthy) ?? null,
      },
    },
  });

  const fields = [
    {
      name: 'condition.types', label: 'Typy událostí', defaultValue: eventTypes,
      options: eventTypes.map((value) => ({ value, label: formatEventType(value) })),
    },
    {
      name: 'condition.participantIds', label: 'Lidé', defaultValue: [],
      options: data?.people?.nodes.map((person) => ({ value: person.id, label: person.name })) ?? [],
    },
    {
      name: 'condition.trainerIds', label: 'Trenéři', defaultValue: [],
      options: data?.tenant?.tenantTrainersList?.flatMap(({ person }) =>
        person ? [{ value: person.id, label: person.name }] : []) ?? [],
    },
    {
      name: 'condition.locationIds', label: 'Místa', defaultValue: [],
      options: data?.tenant?.locationsList?.map((x) => ({ value: x.id, label: x.name })) ?? [],
    },
  ] as const;

  const onSubmit = async (values: z.output<typeof Form>) => {
    const result = savedSearch
      ? await update({ id: savedSearch.id, patch: values })
      : await create({ input: values });
    if (!result.error) onSuccess();
  };
  const feedPath = savedSearch?.feedKey ? `/api/calendar/${savedSearch.feedKey}` : null;

  return (
    <form className="grid gap-3" onSubmit={handleSubmit(onSubmit)}>
      <FormError error={createResult.error ?? updateResult.error ?? error} />
      <TextFieldElement control={control} name="name" label="Název filtru" />

      <div className="flex flex-wrap gap-2">
        {fields.map(({ name, label, options, defaultValue }) => (
          <Controller
            key={name}
            control={control}
            name={name}
            render={({ field, fieldState }) => (
              <div>
                <FacetFilter
                  label={label}
                  value={field.value ?? defaultValue}
                  defaultValue={defaultValue}
                  options={options}
                  onChange={(value) => field.onChange(
                    name === 'condition.types' && value.length === eventTypes.length ? null : value,
                  )}
                />
                <FieldHelper error={fieldState.error} />
              </div>
            )}
          />
        ))}
      </div>
      <p className="text-sm text-neutral-11">Výběr lidí zahrnuje jejich účast i výuku.</p>

      <ComboboxElement
        control={control}
        name="condition.cohortId"
        label="Tréninková skupina"
        placeholder="Bez omezení"
        options={[
          { id: null, label: 'Bez omezení' },
          ...data?.tenant?.cohortsList?.filter((x) => !x.isArchived)
            .map((x) => ({ id: x.id, label: x.name })) ?? [],
        ]}
      />
      <ComboboxElement
        control={control}
        name="condition.parentId"
        label="Součást soustředění"
        placeholder="Bez omezení"
        options={[
          { id: null, label: 'Bez omezení' },
          ...data?.parents?.map((x) => ({
            id: x.id,
            label: `${x.name || 'Soustředění'} – ${fullDateFormatter.formatRange(new Date(x.since), new Date(x.until))}`,
          })) ?? [],
        ]}
      />

      <CheckboxElement control={control} name="condition.onlyMine" label="Pouze moje události" />
      <CheckboxElement control={control} name="condition.relevantOnly" label="Pouze související události" />
      <CheckboxElement control={control} name="condition.canRegister" label="Pouze volné lekce" />
      <CheckboxElement control={control} name="condition.withCompetitions" label="Včetně soutěží" />
      <CheckboxElement
        control={control}
        name="crossTenant"
        label="Kalendář ze všech mých klubů"
      />
      <p className="text-sm text-neutral-11">
        Platí pro odběr kalendáře. Přehled v aplikaci zůstává v aktuálním klubu.
      </p>

      {feedPath && (
        <div className="grid gap-2 border-t border-neutral-4 pt-3">
          <TextField label="Odkaz pro odběr kalendáře" value={feedPath} readOnly />
          <div className="flex flex-wrap gap-2">
            <button
              type="button"
              className={buttonCls({ variant: 'outline', size: 'sm' })}
              onClick={async () => {
                await navigator.clipboard.writeText(`${window.location.origin}${feedPath}`);
                toast.success('Odkaz zkopírován.');
              }}
            >
              <Copy /> Kopírovat odkaz
            </button>
            <a href={feedPath} className={buttonCls({ variant: 'outline', size: 'sm' })}>
              <Download /> Stáhnout ICS
            </a>
          </div>
        </div>
      )}

      <SubmitButton control={control}>
        {savedSearch ? 'Uložit změny' : 'Vytvořit filtr'}
      </SubmitButton>
    </form>
  );
}
