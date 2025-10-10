import type { AnnouncementAudienceRole, UpsertAnnouncementInput } from '@/graphql';
import {
  type AnnouncementFragment,
  AnnouncementAudienceFragment,
  UpsertAnnouncementDocument,
} from '@/graphql/Announcement';
import { Checkbox, CheckboxElement } from '@/ui/fields/checkbox';
import { DatePickerElement } from '@/ui/fields/date';
import { RichTextEditor } from '@/ui/fields/richtext';
import { TextFieldElement } from '@/ui/fields/text';
import { FormError } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import { AnnouncementAudienceBadges } from '@/ui/AnnouncementAudienceBadges';
import { useCohorts } from '@/ui/useCohorts';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useController, useWatch, type Control } from 'react-hook-form';
import { toast } from 'react-toastify';
import { useMutation } from 'urql';
import { truthyFilter } from '../truthyFilter';
import { useZodForm } from '@/lib/use-schema-form';
import { type TypeOf, z } from 'zod';

const ROLE_OPTIONS: {
  value: AnnouncementAudienceRole;
  label: string;
  helperText: string;
}[] = [
  {
    value: 'MEMBER',
    label: 'Členové',
    helperText: 'Zobrazit oznámení pouze členům klubu.',
  },
  {
    value: 'TRAINER',
    label: 'Trenéři',
    helperText: 'Omezit viditelnost na trenéry klubu.',
  },
  {
    value: 'ADMINISTRATOR',
    label: 'Administrátoři',
    helperText: 'Zobrazit pouze správcům systému.',
  },
];

const AUDIENCE_ROLE_VALUES = ['MEMBER', 'TRAINER', 'ADMINISTRATOR'] as const;

const Form = z.object({
  title: z.string().min(1, 'Zadejte nadpis oznámení'),
  body: z.string().default(''),
  isVisible: z.boolean().default(true),
  isSticky: z.boolean().default(false),
  scheduledSince: z.date().nullable().optional(),
  scheduledUntil: z.date().nullable().optional(),
  audienceRoles: z.array(z.enum(AUDIENCE_ROLE_VALUES)).default([]),
  cohortIds: z.array(z.string()).default([]),
});

type FormValues = TypeOf<typeof Form>;

export function AnnouncementForm({ id, data, onSuccess }: {
  id?: string;
  data?: AnnouncementFragment | null;
  onSuccess?: (id: string | undefined) => void;
}) {
  const upsert = useMutation(UpsertAnnouncementDocument)[1];

  const { reset, control, handleSubmit } = useZodForm(Form);
  React.useEffect(() => {
    reset({
      title: data?.title ?? '',
      body: data?.body ?? '',
      isVisible: data ? data.isVisible : true,
      isSticky: data?.isSticky ?? false,
      scheduledSince: data?.scheduledSince ? new Date(data.scheduledSince) : undefined,
      scheduledUntil: data?.scheduledUntil ? new Date(data.scheduledUntil) : undefined,
      audienceRoles: data?.announcementAudiences.nodes.map(x => x.audienceRole).filter(truthyFilter),
      cohortIds: data?.announcementAudiences.nodes.map(x => x.cohortId).filter(truthyFilter),
    });
  }, [data, reset]);


  const onSubmit = useAsyncCallback(async (values: FormValues) => {
    const oldAudiences = [...data?.announcementAudiences.nodes || []];
    const newAudiences: UpsertAnnouncementInput['audiences'] = [];

    for (const cohortId of values.cohortIds) {
      const existing = oldAudiences.findIndex(x => x.cohortId === cohortId);
      if (existing !== -1) {
        newAudiences.push({ cohortId, id: oldAudiences[existing]!.id });
        delete oldAudiences[existing];
      } else {
        newAudiences.push({ cohortId })
      }
    }
    for (const audienceRole of values.audienceRoles) {
      const existing = oldAudiences.findIndex(x => x.audienceRole === audienceRole);
      if (existing !== -1) {
        newAudiences.push({ audienceRole, id: oldAudiences[existing]!.id });
        delete oldAudiences[existing];
      } else {
        newAudiences.push({ audienceRole })
      }
    }
    // Remaining = unselected & to be deleted
    for (const remaining of oldAudiences) {
      newAudiences.push({ id: remaining.id });
    }

    const res = await upsert({
      input: {
        info: {
          id,
          title: values.title,
          body: values.body,
          isVisible: values.isVisible,
          isSticky: values.isSticky,
          scheduledSince: values.scheduledSince?.toISOString(),
          scheduledUntil: values.scheduledUntil?.toISOString(),
        },
        audiences: newAudiences,
      }
    });
    const newId = res.data?.upsertAnnouncement?.announcement?.id;
    if (!id && newId) {
      toast.success('Přidáno.');
    }
    onSuccess?.(newId);
  });

  return (
    <form className="space-y-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <FormError error={onSubmit.error} />

      <TextFieldElement control={control} name="title" label="Nadpis" required />
      <RichTextEditor initialState={data?.body} control={control} name="body" label="Text" />
      <CheckboxElement control={control} name="isVisible" value="1" label="Viditelný" />
      <CheckboxElement control={control} name="isSticky" value="1" label="Připnout na stálou nástěnku" />
      <DatePickerElement control={control} name="scheduledSince" label="Odložit zveřejnění na den" />
      <DatePickerElement control={control} name="scheduledUntil" label="Skrýt příspěvek dne" />
      <AnnouncementAudienceEditor control={control} />

      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
}

function AnnouncementAudienceEditor({ control }: {
  control: Control<FormValues>;
}) {
  const { audienceRoles = [], cohortIds = [] } = useWatch<FormValues>({ control });
  const { data: cohorts, fetching: cohortsLoading } = useCohorts();

  const audiences: AnnouncementAudienceFragment[] = [
    ...audienceRoles.map(x => ({ id: '', cohortId: null, cohort: null, audienceRole: x })),
    ...cohortIds.map(x => ({ id: '', cohortId: x, cohort: cohorts.find(c => c.id === x) || null, audienceRole: null })),
  ];

  const showWarning = audienceRoles.length === 0 && cohortIds.length === 0;

  return (
    <div className="space-y-4 rounded-md border border-neutral-6 bg-neutral-1 p-4">
      <div>
        <h3 className="text-sm font-semibold text-neutral-12">Cílové publikum</h3>
        <p className="text-xs text-neutral-11">
          Určete, které role nebo skupiny mají oznámení vidět.
        </p>
      </div>

      <section className="space-y-2">
        <h4 className="text-xs font-semibold uppercase tracking-wide text-neutral-10">Role</h4>
        <AudienceRoleCheckboxes control={control} />
      </section>

      <section className="space-y-2">
        <h4 className="text-xs font-semibold uppercase tracking-wide text-neutral-10">Skupiny</h4>
        <AudienceCohortCheckboxes control={control} cohorts={cohorts} loading={cohortsLoading} />
      </section>

      <div className="space-y-2">
        <h4 className="text-xs font-semibold uppercase tracking-wide text-neutral-10">Shrnutí</h4>
        <AnnouncementAudienceBadges audiences={audiences} />
        {showWarning ? (
          <div className="rounded-md border border-accent-7 bg-accent-3 px-3 py-2 text-xs text-accent-12">
            Bez výběru konkrétního publika se oznámení zobrazí všem.
          </div>
        ) : null}
      </div>
    </div>
  );
}

function AudienceRoleCheckboxes({ control }: { control: Control<FormValues> }) {
  const { field } = useController({ control, name: 'audienceRoles' });

  const toggle = React.useCallback(
    (role: AnnouncementAudienceRole) => {
      const next = new Set(field.value ?? []);
      if (next.has(role)) {
        next.delete(role);
      } else {
        next.add(role);
      }
      field.onChange([...next]);
    },
    [field],
  );

  return (
    <div className="space-y-1">
      {ROLE_OPTIONS.map((role) => (
        <Checkbox
          key={role.value}
          name={`announcement-role-${role.value}`}
          checked={field.value.includes(role.value)}
          value={role.value}
          label={role.label}
          helperText={role.helperText}
          onChange={(event) => {
            event.stopPropagation();
            toggle(role.value);
          }}
        />
      ))}
    </div>
  );
}

function AudienceCohortCheckboxes({
  control,
  cohorts,
  loading,
}: {
  control: Control<FormValues>;
  cohorts: { id: string; name?: string | null; colorRgb?: string | null }[];
  loading?: boolean;
}) {
  const { field } = useController({ control, name: 'cohortIds' });

  const toggle = React.useCallback(
    (cohortId: string) => {
      const next = new Set(field.value);
      if (next.has(cohortId)) {
        next.delete(cohortId);
      } else {
        next.add(cohortId);
      }
      field.onChange([...next]);
    },
    [field],
  );

  if (loading) {
    return <div className="text-xs text-neutral-11">Načítám skupiny…</div>;
  }

  if (cohorts.length === 0) {
    return <div className="text-xs text-neutral-11">Žádné skupiny nejsou k dispozici.</div>;
  }

  return (
    <div className="space-y-1">
      {cohorts.map((cohort) => {
        const checked = field.value.includes(cohort.id);
        return (
          <Checkbox
            key={cohort.id}
            name={`announcement-cohort-${cohort.id}`}
            checked={checked}
            value={cohort.id}
            label={cohort.name ?? 'Bez názvu'}
            onChange={(event) => {
              event.stopPropagation();
              toggle(cohort.id);
            }}
          />
        );
      })}
    </div>
  );
}
