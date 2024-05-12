import { CreateCoupleDocument } from '@/graphql/Memberships';
import { PersonBasicFragment, PersonListDocument } from '@/graphql/Person';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { ComboboxElement } from '@/ui/fields/Combobox';
import { FormError } from '@/ui/form';
import { buttonCls } from '@/ui/style';
import { SubmitButton } from '@/ui/submit';
import { useAuth } from "@/ui/use-auth";
import { zodResolver } from '@hookform/resolvers/zod';
import { Plus } from 'lucide-react';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useForm } from 'react-hook-form';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';

const Form = z.object({
  man: z.string(),
  woman: z.string(),
});
type FormProps = z.infer<typeof Form>;

export function CreateCoupleForm({ initial, onSuccess }: { initial?: PersonBasicFragment; onSuccess?: () => void }) {
  const [{ data }] = useQuery({ query: PersonListDocument });
  const men = React.useMemo(
    () =>
      (data?.filteredPeopleList || [])
        .filter((x) => x.gender === 'MAN')
        .map((x) => ({
          id: x.id,
          label: x.name + (x.birthDate ? ` (${new Date(x.birthDate).getFullYear()})` : ''),
        })),
    [data],
  );
  const women = React.useMemo(
    () =>
      (data?.filteredPeopleList || [])
        .filter((x) => x.gender === 'WOMAN')
        .map((x) => ({
          id: x.id,
          label: x.name + (x.birthDate ? ` (${new Date(x.birthDate).getFullYear()})` : ''),
        })),
    [data],
  );

  const { reset, control, handleSubmit } = useForm<FormProps>({ resolver: zodResolver(Form) });
  React.useEffect(() => {
    if (initial && initial.gender === 'MAN') {
      reset({ man: initial.id });
    } else if (initial && initial.gender === 'WOMAN') {
      reset({ woman: initial.id });
    }
  }, [initial, reset]);

  const doCreate = useMutation(CreateCoupleDocument)[1];

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    const res = await doCreate({
      input: {
        couple: {
          manId: values.man,
          womanId: values.woman,
          since: new Date().toISOString(),
        },
      },
    });
    const id = res.data?.createCouple?.couple?.id;
    if (id) {
      onSuccess?.();
    }
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <FormError error={onSubmit.error} />
      <ComboboxElement
        control={control}
        name="man"
        label="Partner"
        placeholder="vyberte partnera"
        options={men}
      />
      <ComboboxElement
        control={control}
        name="woman"
        label="Partnerka"
        placeholder="vyberte partnerku"
        options={women}
      />
      <SubmitButton loading={onSubmit.loading}>Spárovat</SubmitButton>
    </form>
  );
}

export function CreateCoupleButton() {
  const auth = useAuth();
  const [open, setOpen] = React.useState(false);

  if (!auth.isAdmin) {
    return;
  }

  return (
    <Dialog open={open} onOpenChange={setOpen} modal={false}>
      <DialogTrigger asChild>
        <button className={buttonCls({ size: 'sm', variant: 'outline' })}>
          <Plus />
          Přidat pár
        </button>
      </DialogTrigger>
      <DialogContent>
        <CreateCoupleForm onSuccess={() => setOpen(false)} />
      </DialogContent>
    </Dialog>
  );
}
