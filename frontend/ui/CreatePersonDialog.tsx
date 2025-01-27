'use client';

import { CreatePersonDocument, FullPersonListDocument, CstsPersonDocument } from '@/graphql/Person';
import { useZodForm } from '@/lib/use-schema-form';
import { RadioButtonGroupElement } from '@/ui/fields/RadioButtonGroupElement';
import { Dialog, DialogContent, DialogTitle } from '@/ui/dialog';
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent, DropdownMenuTrigger } from '@/ui/dropdown';
import { ComboboxElement } from '@/ui/fields/Combobox';
import { CheckboxElement } from '@/ui/fields/checkbox';
import { DatePickerElement } from '@/ui/fields/date';
import { TextFieldElement } from '@/ui/fields/text';
import { buttonCls } from '@/ui/style';
import { SubmitButton } from '@/ui/submit';
import { countries } from '@/lib/countries';
import { Plus } from 'lucide-react';
import { useRouter } from 'next/router';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { toast } from 'react-toastify';
import { useMutation, useQuery } from 'urql';
import { type TypeOf, z } from 'zod';

const Form = z.object({
  prefixTitle: z.string().default(''),
  firstName: z.string(),
  lastName: z.string(),
  suffixTitle: z.string().default(''),
  gender: z.enum(['MAN', 'WOMAN', 'UNSPECIFIED']),
  birthDate: z.string().nullish(),
  cstsId: z
    .string()
    .regex(/[0-9]{8}/, 'Neplatné IDT')
    .nullish(),
  wdsfId: z
    .string()
    .regex(/[0-9]{8}/, 'Neplatný MIN')
    .nullish(),
  taxIdentificationNumber: z
    .string()
    .regex(/[0-9]{9,10}/, 'Neplatné rodné číslo')
    .nullish(),
  nationality: z.string(),
  bio: z.string().default(''),
  email: z.string().email().optional(),
  phone: z.string().optional(),
  personId: z.string().nullish().default(null),
  isMember: z.boolean().default(false),
  isTrainer: z.boolean().default(false),
  isAdmin: z.boolean().default(false),
  sendInvitation: z.boolean().default(false),
  joinDate: z.date(),
});

export function CreatePersonDialog() {
  const [open, setOpen] = React.useState<'existing' | 'new' | null>(null);
  const router = useRouter();
  const create = useMutation(CreatePersonDocument)[1];

  const [personQuery] = useQuery({ query: FullPersonListDocument, pause: open !== 'existing' });
  const personOptions = React.useMemo(() => personQuery.data?.people?.nodes?.map(x => ({
    id: x.id,
    label: x.name || '?',
  })).sort((x, y) => x.label.localeCompare(y.label)), [personQuery]);

  const { control, handleSubmit, getValues, setValue, reset, watch } = useZodForm(Form);

  const [cstsId, setCstsId] = React.useState(NaN);
  const [cstsQuery] = useQuery({
    query: CstsPersonDocument,
    pause: isNaN(cstsId),
    variables: { idt: cstsId },
  });

  const personId = watch('personId');
  React.useEffect(() => {
    const person = personQuery.data?.people?.nodes.find(x => x.id === personId);
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
    }
  }, [setValue, personId]);

  const email = watch('email');
  React.useEffect(() => {
    if (email && !getValues('sendInvitation')) {
      setValue('sendInvitation', true);
    }
  }, [email, getValues, setValue]);

  React.useEffect(() => {
    if (open) {
      reset();
      setCstsId(NaN);
      setValue('isMember', true);
      setValue('sendInvitation', false);
      setValue('nationality', "203");
      setValue('joinDate', new Date());
    }
  }, [open, reset, setValue])

  const onSubmit = useAsyncCallback(async (data: TypeOf<typeof Form>) => {
    const { personId, isAdmin, isMember, isTrainer, joinDate, sendInvitation, ...p } = data;
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
          <DropdownMenuButton onClick={() => setOpen('new')}>Nová osoba</DropdownMenuButton>
          <DropdownMenuButton onClick={() => setOpen('existing')}>Z jiného klubu</DropdownMenuButton>
        </DropdownMenuContent>
      </DropdownMenu>

      <DialogContent className="sm:max-w-2xl" onPointerDownOutside={(e) => e.preventDefault()}>
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
            <TextFieldElement control={control} name="prefixTitle" label="Titul před jménem" />
            <TextFieldElement control={control} name="suffixTitle" label="Titul za jménem" />
            <TextFieldElement control={control} name="firstName" label="Jméno" required autoFocus />
            <TextFieldElement control={control} name="lastName" label="Příjmení" required />

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

            <div>
              <TextFieldElement
                control={control}
                name="cstsId"
                label="ČSTS IDT"
                placeholder="10000000"
                onInput={(e) => setCstsId(parseInt(e.currentTarget.value || '', 10))}
              />
              {cstsQuery.data ? (
                cstsQuery.data.cstsAthlete ? (
                  <span className="text-green-9">{cstsQuery.data?.cstsAthlete.name}</span>
                ) : (
                  <span className="text-primary">Nenalezeno</span>
                )
              ) : null}
            </div>

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
            <TextFieldElement control={control} name="email" label="E-mail" type="email" />
          </fieldset>

          <div className="grid lg:grid-cols-2 gap-2">
            <div>
              <CheckboxElement control={control} name="isMember" label="Člen klubu" />
              <CheckboxElement control={control} name="isTrainer" label="Trenér" />
              <CheckboxElement control={control} name="isAdmin" label="Správce" />
              <CheckboxElement control={control} name="sendInvitation" label="Poslat pozvánku na e-mail" />
            </div>

            <DatePickerElement control={control} name="joinDate" label="Datum vstupu do klubu" />
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
};
