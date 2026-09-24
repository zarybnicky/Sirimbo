import {
  ConfirmMembershipApplicationDocument,
  CreateMembershipApplicationDocument,
  DeleteMembershipApplicationDocument,
  type MembershipApplicationFragment,
  UpdateMembershipApplicationDocument,
} from '@/graphql/MembershipApplication';
import { CohortListDocument } from '@/graphql/Cohorts';
import {
  RadioButtonGroupElement,
  VerticalCheckboxButtonGroupElement,
} from '@/ui/fields/RadioButtonGroupElement';
import { ComboboxElement } from '@/ui/fields/Combobox';
import { CheckboxElement } from '@/ui/fields/checkbox';
import { DatePickerElement } from '@/ui/fields/date';
import { TextFieldElement } from '@/ui/fields/text';
import { TextAreaElement } from '@/ui/fields/textarea';
import { CstsIdFieldElement } from '@/ui/fields/CstsIdFieldElement';
import { FieldLabel, FormError, useFormResult } from '@/ui/form';
import { buttonCls } from '@/ui/style';
import { SubmitButton } from '@/ui/submit';
import { useAuth } from '@/lib/auth';
import { countryOptions } from '@/lib/countries';
import { parseCzechBirthNumber } from '@/lib/czechBirthNumber';
import * as Collapsible from '@radix-ui/react-collapsible';
import { Check, ChevronDown, Trash2 } from 'lucide-react';
import React from 'react';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';
import { useForm, useWatch } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { sanitizeUnicode } from '@/lib/sanitize';

const ApplicationForm = z.object({
  prefixTitle: z.string().prefault('').overwrite(sanitizeUnicode),
  firstName: z.string({ error: 'Zadejte jméno' }).min(1, 'Zadejte jméno').overwrite(sanitizeUnicode),
  lastName: z.string({ error: 'Zadejte příjmení' }).min(1, 'Zadejte příjmení').overwrite(sanitizeUnicode),
  suffixTitle: z.string().prefault('').overwrite(sanitizeUnicode),
  gender: z.enum(['MAN', 'WOMAN', 'UNSPECIFIED'], { error: 'Vyberte pohlaví' }),
  birthDate: z.string().nullish(),
  email: z.email({ error: 'Zadejte platný e-mail' }),
  phone: z.preprocess(
    (value) => (value === '' ? null : value),
    z.string().min(9).max(14).nullish(),
  ),
  cstsId: z.number().int().positive().nullable().optional(),
  wdsfId: z.number().int().positive().nullable().optional(),
  taxIdentificationNumber: z
    .string()
    .regex(/^(?:\d{9,10})?$/, 'Neplatné rodné číslo')
    .nullish(),
  nationality: z.string(),
  note: z.string().prefault(''),
});

const Form = ApplicationForm.extend({
  isMember: z.boolean(),
  isTrainer: z.boolean(),
  isAdmin: z.boolean(),
  joinDate: z.date(),
  cohortIds: z.array(z.string()),
});

export function CreateMembershipApplicationForm({
  data,
  onCreate,
  onRemove,
}: {
  data?: MembershipApplicationFragment;
  onCreate?: (id: string) => void;
  onRemove?: () => void;
}) {
  const { onSuccess } = useFormResult();
  const auth = useAuth();
  const { control, handleSubmit, getValues, setValue } = useForm({
    resolver: zodResolver(Form),
    defaultValues: {
      ...(data
        ? ApplicationForm.partial().parse({ ...data, email: data.email ?? '' })
        : { email: auth.user?.uEmail ?? '', note: '' }),
      isMember: true,
      isTrainer: false,
      isAdmin: false,
      joinDate: new Date(),
      cohortIds: [],
    },
  });

  const fillBirthDate = () => {
    if (getValues('birthDate')) return;
    const birthDate = parseCzechBirthNumber(getValues('taxIdentificationNumber'));
    if (birthDate) setValue('birthDate', birthDate, { shouldDirty: true });
  };
  const [createResult, create] = useMutation(CreateMembershipApplicationDocument);
  const [updateResult, update] = useMutation(UpdateMembershipApplicationDocument);
  const [confirmResult, confirm] = useMutation(ConfirmMembershipApplicationDocument);
  const [deleteResult, del] = useMutation(DeleteMembershipApplicationDocument);
  const [{ data: cohorts }] = useQuery({
    query: CohortListDocument,
    variables: { archived: false },
    pause: !data || !auth.isAdmin,
  });
  const cohortOptions = React.useMemo(
    () => cohorts?.cohortsList?.map((cohort) => ({ id: cohort.id, label: cohort.name })) ?? [],
    [cohorts],
  );
  const selectedCohortCount = useWatch({ control, name: 'cohortIds' })?.length ?? 0;
  const [cohortPickerOpen, setCohortPickerOpen] = React.useState(false);

  const onSubmit = async (values: z.infer<typeof Form>) => {
    if (!auth.user) return;
    const application = ApplicationForm.parse(values);

    let result;
    let createdId: string | undefined;
    if (data) {
      result = await update({ input: { id: data.id, patch: application } });
    } else {
      result = await create({
        input: {
          membershipApplication: {
            ...application,
            createdBy: auth.user.id,
          },
        },
      });
      createdId = result.data?.createMembershipApplication?.membershipApplication?.id;
    }
    if (!result.error) {
      onSuccess();
      if (createdId) onCreate?.(createdId);
    }
  };

  const onConfirm = async (values: z.infer<typeof Form>) => {
    if (!data) return;

    const application = ApplicationForm.parse(values);
    const updateResult = await update({ input: { id: data.id, patch: application } });
    if (updateResult.error) return;

    const result = await confirm({
      input: {
        applicationId: data.id,
        isMember: values.isMember,
        isTrainer: values.isTrainer,
        isAdmin: values.isAdmin,
        joinDate: values.joinDate.toISOString(),
        cohortIds: values.cohortIds,
      },
    });
    if (!result.error) {
      onSuccess();
      onRemove?.();
    }
  };

  const onDelete = async () => {
    if (!data) return;
    const result = await del({ input: { id: data.id } });
    if (!result.error) {
      onSuccess();
      onRemove?.();
    }
  };

  return (
    <form onSubmit={handleSubmit(onSubmit)}>
      <fieldset className="grid lg:grid-cols-2 gap-2">
        <FormError
          error={
            createResult.error ||
            updateResult.error ||
            confirmResult.error ||
            deleteResult.error
          }
        />

        <TextFieldElement
          control={control}
          name="prefixTitle"
          label="Titul před jménem"
        />
        <TextFieldElement control={control} name="suffixTitle" label="Titul za jménem" />
        <TextFieldElement
          control={control}
          name="firstName"
          label="Jméno"
          required
          autoFocus
        />
        <TextFieldElement control={control} name="lastName" label="Příjmení" required />

        <TextFieldElement
          control={control}
          name="email"
          type="email"
          label="E-mail"
          autoComplete="email"
          required
        />
        <TextFieldElement
          control={control}
          name="phone"
          type="tel"
          label="Telefon"
          autoComplete="tel"
        />

        <DatePickerElement
          control={control}
          label="Datum narození"
          name="birthDate"
          valueMode="date"
        />
        <TextFieldElement
          control={control}
          name="taxIdentificationNumber"
          label="Rodné číslo"
          placeholder="1111119999"
          inputMode="numeric"
          onBlur={fillBirthDate}
        />

        <CstsIdFieldElement control={control} name="cstsId" />
        <TextFieldElement
          control={control}
          name="wdsfId"
          type="number"
          label="WDSF MIN"
          placeholder="10000000"
        />

        <div className="col-full">
          <RadioButtonGroupElement
            control={control}
            name="gender"
            options={[
              { id: 'MAN', label: 'Muž' },
              { id: 'WOMAN', label: 'Žena' },
            ]}
          />
        </div>

        <div className="col-full">
          <ComboboxElement
            control={control}
            label="Národnost"
            name="nationality"
            placeholder="vyberte národnost"
            options={countryOptions}
          />
        </div>

        <div className="col-full">
          <TextAreaElement
            control={control}
            name="note"
            label="Poznámka k přihlášce"
          />
        </div>

        {data && auth.isAdmin && (
          <div className="col-full grid gap-2 border-t border-neutral-6 pt-3 lg:grid-cols-2">
            <div>
              <CheckboxElement
                control={control}
                name="isMember"
                label="Člen klubu"
              />
              <CheckboxElement control={control} name="isTrainer" label="Trenér" />
              <CheckboxElement control={control} name="isAdmin" label="Správce" />
            </div>

            <div className="grid gap-2">
              <DatePickerElement
                control={control}
                name="joinDate"
                label="Datum vstupu do klubu"
              />

              <Collapsible.Root
                open={cohortPickerOpen}
                onOpenChange={setCohortPickerOpen}
              >
                <div className="flex items-center justify-between gap-2">
                  <FieldLabel htmlFor="cohortIds">Tréninkové skupiny</FieldLabel>
                  <Collapsible.Trigger asChild>
                    <button
                      type="button"
                      className={`${buttonCls({ size: 'xs', variant: 'outline' })} gap-1`}
                      aria-expanded={cohortPickerOpen}
                      aria-controls="membership-application-cohorts"
                    >
                      <span>
                        {cohortPickerOpen
                          ? 'Skrýt'
                          : selectedCohortCount > 0
                            ? `Vybráno ${selectedCohortCount}`
                            : 'Zobrazit'}
                      </span>
                      <ChevronDown
                        className={`transition-transform ${cohortPickerOpen ? 'rotate-180' : ''}`}
                      />
                    </button>
                  </Collapsible.Trigger>
                </div>
                <Collapsible.Content
                  forceMount
                  id="membership-application-cohorts"
                  className="[&[data-state=closed]>div]:hidden"
                >
                  <VerticalCheckboxButtonGroupElement
                    control={control}
                    name="cohortIds"
                    options={cohortOptions}
                    className="mt-2"
                  />
                </Collapsible.Content>
              </Collapsible.Root>
            </div>
          </div>
        )}
      </fieldset>

      <div className="col-full flex flex-wrap justify-between gap-3 pt-2">
        <div className="flex gap-2">
          {data && auth.isAdmin && (
            <button
              className={buttonCls()}
              type="button"
              onClick={handleSubmit(onConfirm)}
            >
              <Check />
              Potvrdit jako člena
            </button>
          )}

          <SubmitButton
            control={control}
            variant={data && auth.isAdmin ? 'outline' : undefined}
          >
            {data ? 'Uložit změny' : 'Odeslat přihlášku'}
          </SubmitButton>
        </div>

        {data && (
          <button
            type="button"
            onClick={onDelete}
            className={buttonCls({ variant: 'outline' })}
          >
            <Trash2 />
            Smazat přihlášku
          </button>
        )}
      </div>
    </form>
  );
}
