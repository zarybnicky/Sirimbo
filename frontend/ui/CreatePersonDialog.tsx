'use client';

import { CreatePersonDocument, FullPersonListDocument } from '@/graphql/Person';
import { CohortListDocument, SyncCohortMembershipsDocument } from '@/graphql/Cohorts';
import {
  RadioButtonGroupElement,
  VerticalCheckboxButtonGroupElement,
} from '@/ui/fields/RadioButtonGroupElement';
import { Dialog, DialogContent, DialogTitle } from '@/ui/dialog';
import {
  DropdownMenu,
  DropdownMenuButton,
  DropdownMenuContent,
  DropdownMenuTrigger,
} from '@/ui/dropdown';
import { ComboboxElement } from '@/ui/fields/Combobox';
import { CheckboxElement } from '@/ui/fields/checkbox';
import { DatePickerElement } from '@/ui/fields/date';
import { TextFieldElement } from '@/ui/fields/text';
import { CstsIdFieldElement } from '@/ui/fields/CstsIdFieldElement';
import { buttonCls } from '@/ui/style';
import { SubmitButton } from '@/ui/submit';
import { countries } from '@/lib/countries';
import * as Collapsible from '@radix-ui/react-collapsible';
import { FieldLabel } from '@/ui/form';
import { ChevronDown, Plus } from 'lucide-react';
import { useRouter } from 'next/router';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { toast } from 'react-toastify';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';
import { isTruthy } from './truthyFilter';
import { useForm, useWatch } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';

const Form = z.object({
  prefixTitle: z.string().prefault(''),
  firstName: z.string(),
  lastName: z.string(),
  suffixTitle: z.string().prefault(''),
  gender: z.enum(['MAN', 'WOMAN', 'UNSPECIFIED']),
  birthDate: z.string().nullish(),
  cstsId: z
    .string()
    .regex(/^$|\d{8}/, 'Neplatné IDT')
    .nullish(),
  wdsfId: z
    .string()
    .regex(/^$|\d{8}/, 'Neplatný MIN')
    .nullish(),
  taxIdentificationNumber: z
    .string()
    .regex(/^$|\d{9,10}/, 'Neplatné rodné číslo')
    .nullish(),
  nationality: z.string(),
  bio: z.string().prefault(''),
  email: z.email().optional(),
  phone: z.string().optional(),
  personId: z.string().nullish().prefault(null),
  isMember: z.boolean().prefault(false),
  isTrainer: z.boolean().prefault(false),
  isAdmin: z.boolean().prefault(false),
  sendInvitation: z.boolean().prefault(false),
  joinDate: z.date(),
  cohortIds: z.array(z.string()).prefault([]),
});

export function CreatePersonDialog() {
  const [open, setOpen] = React.useState<'existing' | 'new' | null>(null);
  const router = useRouter();
  const create = useMutation(CreatePersonDocument)[1];
  const syncCohorts = useMutation(SyncCohortMembershipsDocument)[1];

  const [personQuery] = useQuery({
    query: FullPersonListDocument,
    pause: open !== 'existing',
  });
  const personOptions = React.useMemo(
    () =>
      personQuery.data?.people?.nodes
        ?.map((x) => ({
          id: x.id,
          label: x.name || '?',
        }))
        .toSorted((x, y) => x.label.localeCompare(y.label)),
    [personQuery],
  );

  const { control, handleSubmit, getValues, setValue, reset } = useForm({
    resolver: zodResolver(Form),
  });
  const [{ data: cohorts }] = useQuery({
    query: CohortListDocument,
    variables: { visible: true },
  });
  const cohortOptions = React.useMemo(
    () => cohorts?.cohortsList?.map((x) => ({ id: x.id, label: x.name })) || [],
    [cohorts],
  );

  const personId = useWatch({ control, name: 'personId' });
  const selectedCohortCount = useWatch({ control, name: 'cohortIds' })?.length ?? 0;
  const [cohortPickerOpen, setCohortPickerOpen] = React.useState(false);
  React.useEffect(() => {
    const person = personQuery.data?.people?.nodes.find((x) => x.id === personId);
    if (person) {
      setValue('prefixTitle', person.prefixTitle);
      setValue('suffixTitle', person.suffixTitle);
      setValue('firstName', person.firstName);
      setValue('lastName', person.lastName);
      setValue('birthDate', person.birthDate);
      setValue('taxIdentificationNumber', person.taxIdentificationNumber);
      setValue('cstsId', person.cstsId);
      setValue('wdsfId', person.wdsfId);
      setValue('nationality', person.nationality);
      setValue('gender', person.gender);
      setValue('phone', person.phone || undefined);
      setValue('email', person.email || undefined);
      setValue('sendInvitation', false);
      setValue('cohortIds', person.cohortIds?.filter(isTruthy) ?? []);
      setCohortPickerOpen((person.cohortIds?.filter(isTruthy) ?? []).length > 0);
    }
  }, [setValue, personId, setCohortPickerOpen, personQuery.data?.people?.nodes]);

  const email = useWatch({ control, name: 'email' });
  React.useEffect(() => {
    if (email && !getValues('sendInvitation')) {
      setValue('sendInvitation', true);
    }
  }, [email, getValues, setValue]);

  React.useEffect(() => {
    if (open) {
      reset(
        {},
        {
          keepDirtyValues: true,
          keepTouched: true,
          keepErrors: true,
        },
      );
      setValue('isMember', true);
      setValue('sendInvitation', false);
      setValue('nationality', '203');
      setValue('joinDate', new Date());
      setValue('cohortIds', []);
      setCohortPickerOpen(false);
    }
  }, [open, reset, setValue, setCohortPickerOpen]);

  const onSubmit = useAsyncCallback(async (data: z.infer<typeof Form>) => {
    const {
      personId,
      isAdmin,
      isMember,
      isTrainer,
      joinDate,
      sendInvitation,
      cohortIds,
      ...p
    } = data;
    const sanitizedCohortIds = (cohortIds ?? []).filter(isTruthy);
    const res = await create({
      input: {
        personId,
        p,
        sendInvitation,
        isAdmin,
        isMember,
        isTrainer,
        joinDate: joinDate.toISOString(),
      },
    });
    const id = res.data?.createPerson?.p?.id;
    if (id) {
      await syncCohorts({
        input: {
          personId: id,
          cohortIds: sanitizedCohortIds,
        },
      });
      toast.success('Přidáno.');
      setOpen(null);
      router.replace({
        pathname: '/clenove/[id]',
        query: { id },
      });
    }
  });

  return (
    <Dialog open={!!open} onOpenChange={() => setOpen(null)}>
      <DropdownMenu>
        <DropdownMenuTrigger className={buttonCls({ variant: 'outline', size: 'sm' })}>
          <Plus />
          Přidat osobu
        </DropdownMenuTrigger>
        <DropdownMenuContent align="end">
          <DropdownMenuButton onClick={() => setOpen('new')}>
            Nová osoba
          </DropdownMenuButton>
          <DropdownMenuButton onClick={() => setOpen('existing')}>
            Z jiného klubu
          </DropdownMenuButton>
        </DropdownMenuContent>
      </DropdownMenu>

      <DialogContent
        className="sm:max-w-2xl"
        onPointerDownOutside={(e) => e.preventDefault()}
      >
        <DialogTitle>Nový člen</DialogTitle>

        <form onSubmit={handleSubmit(onSubmit.execute)}>
          {open === 'existing' && (
            <ComboboxElement
              control={control}
              className="col-full"
              name="personId"
              label="Existující osoba"
              placeholder="vyberte osobu"
              options={personOptions}
            />
          )}
          <fieldset disabled={open === 'existing'} className="grid lg:grid-cols-2 gap-2">
            <TextFieldElement
              control={control}
              name="prefixTitle"
              label="Titul před jménem"
            />
            <TextFieldElement
              control={control}
              name="suffixTitle"
              label="Titul za jménem"
            />
            <TextFieldElement
              control={control}
              name="firstName"
              label="Jméno"
              required
              autoFocus
            />
            <TextFieldElement
              control={control}
              name="lastName"
              label="Příjmení"
              required
            />

            <TextFieldElement
              type="date"
              control={control}
              label="Datum narození"
              name="birthDate"
            />
            <TextFieldElement
              control={control}
              name="taxIdentificationNumber"
              label="Rodné číslo"
              placeholder="1111119999"
            />

            <CstsIdFieldElement control={control} name="cstsId" />

            <TextFieldElement
              control={control}
              name="wdsfId"
              label="WDSF MIN"
              placeholder="10000000"
            />

            <ComboboxElement
              control={control}
              label="Národnost"
              name="nationality"
              placeholder="vyberte národnost"
              options={countries.map((x) => ({ id: x.code.toString(), label: x.label }))}
            />
            <RadioButtonGroupElement
              control={control}
              name="gender"
              label="Pohlaví"
              options={[
                { id: 'MAN', label: 'Muž' },
                { id: 'WOMAN', label: 'Žena' },
              ]}
            />

            <TextFieldElement control={control} name="phone" label="Telefon" type="tel" />
            <TextFieldElement
              control={control}
              name="email"
              label="E-mail"
              type="email"
            />
          </fieldset>

          <div className="grid lg:grid-cols-2 gap-2">
            <div>
              <CheckboxElement control={control} name="isMember" label="Člen klubu" />
              <CheckboxElement control={control} name="isTrainer" label="Trenér" />
              <CheckboxElement control={control} name="isAdmin" label="Správce" />
              <CheckboxElement
                control={control}
                name="sendInvitation"
                label="Poslat pozvánku na e-mail"
              />
            </div>

            <DatePickerElement
              control={control}
              name="joinDate"
              label="Datum vstupu do klubu"
            />
            <Collapsible.Root open={cohortPickerOpen} onOpenChange={setCohortPickerOpen}>
              <div className="flex items-center justify-between gap-2">
                <FieldLabel htmlFor="cohortIds">Tréninkové skupiny</FieldLabel>
                <Collapsible.Trigger asChild>
                  <button
                    type="button"
                    className={`${buttonCls({ size: 'xs', variant: 'outline' })} gap-1`}
                    aria-expanded={cohortPickerOpen}
                    aria-controls="cohortIds-collapsible"
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

          <div className="col-span-2">
            <SubmitButton className="w-full" loading={onSubmit.loading}>
              Vytvořit
            </SubmitButton>
          </div>
        </form>
      </DialogContent>
    </Dialog>
  );
}
