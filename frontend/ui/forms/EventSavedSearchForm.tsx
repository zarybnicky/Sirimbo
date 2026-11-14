import { eventTypes, savedSearchAtom } from '@/calendar/state';
import {
  automaticSearchNamePrefix,
  suggestSavedSearchName,
} from '@/calendar/savedSearchName';
import { calendarTypeOptions, calendarTypes } from '@/calendar/useCalendarData';
import { useAtom } from 'jotai';
import type { EventSavedSearchInput } from '@/graphql';
import {
  CreateEventSavedSearchDocument,
  type EventSavedSearchFragment,
  UpdateEventSavedSearchDocument,
} from '@/graphql/Event';
import { isTruthy } from '@/lib/truthyFilter';
import { useAuth } from '@/lib/auth';
import { FacetFilter } from '@/ui/FacetFilter';
import { CheckboxElement } from '@/ui/fields/checkbox';
import {
  SelectionElement,
  SelectionList,
  SelectionListElement,
} from '@/ui/fields/SelectionList';
import { TextField, TextFieldElement } from '@/ui/fields/text';
import { FieldHelper, FormError, useFormResult } from '@/ui/form';
import { listFormatter } from '@/ui/format';
import { useEventFormOptions } from '@/ui/event-form/useEventFormOptions';
import { buttonCls } from '@/ui/style';
import { SubmitButton } from '@/ui/submit';
import { zodResolver } from '@hookform/resolvers/zod';
import { Copy } from 'lucide-react';
import { useMemo } from 'react';
import { Controller, useFieldArray, useForm, useWatch } from 'react-hook-form';
import { useMutation } from 'urql';
import { z } from 'zod';

const optionalIds = z
  .array(z.string())
  .nullish()
  .transform((xs) => (xs?.length ? xs : null));

const Form = z.object({
  people: z
    .array(
      z.object({
        personId: z.string().nullable(),
        coupleId: z.string().nullable(),
        onlyTeaching: z.boolean().prefault(false),
      }),
    )
    .prefault([]),
  name: z.string().trim(),
  crossTenant: z.boolean().prefault(false),
  condition: z
    .object({
      types: z.array(z.enum(calendarTypes)),
      locationIds: optionalIds,
      parentId: z.string().nullable().prefault(null),
      cohortId: z.string().nullable().prefault(null),
      onlyMine: z.boolean().nullable().prefault(false),
      canRegister: z
        .boolean()
        .nullish()
        .transform((x) => x || null),
    })
    .prefault({ types: [...eventTypes] })
    .transform((condition) => ({
      ...condition,
      relevantOnly: !condition.parentId && !condition.locationIds?.length,
    })),
});

function toSavedSearchInput(values: z.output<typeof Form>) {
  const { people, ...search } = values;
  const { types, canRegister } = search.condition;
  const participants = people.flatMap((x) =>
    x.personId && !x.onlyTeaching ? x.personId : [],
  );
  const trainers = people.flatMap((x) =>
    x.personId && x.onlyTeaching ? x.personId : [],
  );
  return {
    ...search,
    condition: {
      ...search.condition,
      participantIds: participants.length > 0 ? participants : null,
      trainerIds: trainers.length > 0 ? trainers : null,
      coupleId: people.find((x) => x.coupleId)?.coupleId ?? null,
      types: eventTypes.every((x) => types.includes(x))
        ? null
        : types.filter((x) => x !== 'COMPETITION' && x !== 'BIRTHDAY'),
      withCompetitions: !canRegister && types.includes('COMPETITION'),
      withBirthdays: !canRegister && types.includes('BIRTHDAY'),
    },
  } satisfies EventSavedSearchInput;
}

export function EventSavedSearchForm({
  savedSearch,
}: {
  savedSearch?: EventSavedSearchFragment;
}) {
  const { onSuccess } = useFormResult();
  const auth = useAuth();
  const [selectedSearch, setSelectedSearch] = useAtom(savedSearchAtom);
  const options = useEventFormOptions({
    withParents: !!savedSearch?.condition?.parentId,
    withPeople: true,
    peopleCondition: null,
  });
  const { data, error } = options;
  const [createResult, create] = useMutation(CreateEventSavedSearchDocument);
  const [updateResult, update] = useMutation(UpdateEventSavedSearchDocument);
  const record =
    createResult.data?.createEventSavedSearch?.eventSavedSearch ?? savedSearch;
  const condition = savedSearch?.condition;
  const defaultValues = useMemo(() => {
    const condition = savedSearch?.condition;
    return {
      name: savedSearch?.name.startsWith(automaticSearchNamePrefix)
        ? ''
        : (savedSearch?.name ?? ''),
      crossTenant: savedSearch?.crossTenant ?? false,
      condition: {
        onlyMine: true,
        ...condition,
        types: calendarTypes.filter((type) =>
          type === 'COMPETITION'
            ? !condition || condition.withCompetitions
            : type === 'BIRTHDAY'
              ? condition?.withBirthdays
              : !condition?.types || condition.types.includes(type),
        ),
        locationIds: condition?.locationIds?.filter(isTruthy) ?? null,
      },
      people: [
        ...[
          ...new Set([
            ...(condition?.participantIds?.filter(isTruthy) ?? []),
            ...(condition?.trainerIds?.filter(isTruthy) ?? []),
          ]),
        ].map((personId) => ({
          personId,
          coupleId: null,
          onlyTeaching: condition?.trainerIds?.includes(personId) ?? false,
        })),
        ...(condition?.coupleId
          ? [
              {
                personId: null,
                coupleId: condition.coupleId,
                onlyTeaching: false,
              },
            ]
          : []),
      ],
    };
  }, [savedSearch]);
  const { control, handleSubmit } = useForm({
    resolver: zodResolver(Form),
    defaultValues,
  });
  const { fields: people, append, remove } = useFieldArray({ control, name: 'people' });

  const hasCouple = people.some((x) => x.coupleId);

  const onSubmit = async (values: z.output<typeof Form>) => {
    const input = toSavedSearchInput(values);
    input.name ||= automaticSearchNamePrefix + suggestSavedSearchName(input, data);
    if (!record) {
      await create({ input });
      return;
    }
    const result = await update({ id: record.id, patch: input });
    if (result.error) return;
    const updated = result.data?.updateEventSavedSearch?.eventSavedSearch;
    if (updated && selectedSearch?.id === updated.id) setSelectedSearch(updated);
    onSuccess();
  };

  const liveValues = useWatch({ control });
  const placeholder = suggestSavedSearchName(
    toSavedSearchInput(Form.parse({ ...liveValues, name: '' })),
    data,
  );

  const feedUrl =
    record?.feedKey && typeof window !== 'undefined'
      ? `${window.location.origin}/api/calendar/${record.feedKey}`
      : '';

  return (
    <form className="grid gap-1" onSubmit={handleSubmit(onSubmit)}>
      <FormError error={createResult.error ?? updateResult.error ?? error} />
      <TextFieldElement
        control={control}
        name="name"
        label="Popis"
        inputClassName="placeholder:text-neutral-11 placeholder:opacity-100"
        placeholder={placeholder}
      />

      <div className="grid grid-cols-1 gap-2">
        <Controller
          control={control}
          name="condition.types"
          render={({ field, fieldState }) => (
            <div className="col-span-full grid grid-cols-subgrid gap-y-1">
              <FacetFilter
                inline
                label="Typ"
                value={field.value ?? calendarTypes}
                defaultValue={calendarTypes}
                options={calendarTypeOptions}
                onChange={field.onChange}
              />
              <FieldHelper error={fieldState.error} />
            </div>
          )}
        />
      </div>

      <SelectionListElement
        control={control}
        name="condition.locationIds"
        label="Místa"
        emptyText="Bez omezení místa"
        options={options.locations}
      />
      <SelectionElement
        control={control}
        name="condition.cohortId"
        label="Tréninková skupina"
        emptyText="Bez omezení skupiny"
        options={options.cohorts}
      />

      <SelectionList
        label="Lidé a páry"
        options={[...options.participants.values()].map((x) => ({
          ...x,
          hidden: hasCouple && !!x.coupleId,
        }))}
        value={people.map((x) =>
          x.personId ? `person:${x.personId}` : `couple:${x.coupleId}`,
        )}
        onAdd={(id) => {
          const person = options.participants.get(id);
          if (person)
            append({
              personId: person.personId,
              coupleId: person.coupleId,
              onlyTeaching: false,
            });
        }}
        onRemove={remove}
        renderItem={(_, index) =>
          people[index]?.personId && (
            <CheckboxElement
              control={control}
              name={`people.${index}.onlyTeaching`}
              label="Pouze výuka"
            />
          )
        }
        emptyText={
          !liveValues.condition?.onlyMine
            ? 'Bez omezení lidí a párů'
            : auth.persons.length > 0
              ? `Moje osoby: ${listFormatter.format(auth.persons.map((person) => person.name))}`
              : 'Nemáte přiřazené žádné osoby.'
        }
      >
        <CheckboxElement
          control={control}
          name="condition.onlyMine"
          label="Pouze moje události"
        />
      </SelectionList>

      {condition?.parentId && (
        <SelectionElement
          control={control}
          name="condition.parentId"
          label="Součást soustředění"
          emptyText="Bez omezení"
          options={options.parents}
        />
      )}
      {condition?.canRegister && (
        <CheckboxElement
          control={control}
          name="condition.canRegister"
          label="Pouze volné lekce"
        />
      )}

      <fieldset className="mt-3 grid gap-2 rounded-md border border-neutral-4 bg-neutral-2 p-3">
        <legend className="px-1 text-sm font-semibold">Externí kalendář / export</legend>
        <CheckboxElement
          control={control}
          name="crossTenant"
          label="Kalendář ze všech klubů"
        />

        {record?.feedKey && (
          <div className="grid gap-2">
            <TextField value={feedUrl} readOnly />
            <div className="flex flex-wrap gap-2">
              <button
                type="button"
                className={buttonCls({ variant: 'outline', size: 'sm' })}
                onClick={async () => {
                  await navigator.clipboard.writeText(feedUrl);
                }}
              >
                <Copy /> Kopírovat odkaz
              </button>
              <a
                href={feedUrl.replace(/^https?:/, 'webcal:')}
                className={buttonCls({ variant: 'outline', size: 'sm' })}
              >
                Přidat do Apple Kalendáře
              </a>
            </div>
            <details>
              <summary>Jak přidat do Google Kalendáře</summary>
              <ol className="list-decimal pl-5 space-y-1">
                <li>Na počítači zkopírujte odkaz tlačítkem výše.</li>
                <li>
                  <a
                    href="https://calendar.google.com/calendar/u/0/r/settings/addbyurl"
                    target="_blank"
                    rel="noreferrer"
                    className="text-accent-11 underline"
                  >
                    Otevřete přidání kalendáře v Googlu.
                  </a>
                </li>
                <li>
                  Vložte odkaz do „Adresa URL kalendáře“ a potvrďte „Přidat kalendář“.
                </li>
                <li>
                  V telefonu použijte stejný účet Google a zapněte zobrazení přidaného
                  kalendáře.
                </li>
              </ol>
            </details>
            <details>
              <summary>Jak přidat do Apple Kalendáře</summary>
              <p>
                Klikněte na tlačítko výše a potvrďte odběr. Pokud se aplikace neotevře, na
                iPhonu zvolte Kalendář → Kalendáře → Přidat kalendář → Přidat odebíraný
                kalendář a vložte odkaz.
              </p>
            </details>
          </div>
        )}
      </fieldset>

      <SubmitButton control={control}>{record ? 'Uložit' : 'Vytvořit'}</SubmitButton>
    </form>
  );
}
