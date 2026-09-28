import { LocationOptionsDocument } from '@/graphql/Location';
import { useAuth } from '@/lib/auth';
import {
  DropdownMenu,
  DropdownMenuButton,
  DropdownMenuContent,
  DropdownMenuTrigger,
} from '@/ui/dropdown';
import { buttonCls } from '@/ui/style';
import { CheckCircle2, Circle, MapPin } from 'lucide-react';
import { useQuery } from 'urql';

export function LocationFilter({
  value,
  onChange,
}: {
  value: string | null;
  onChange: (id: string | null) => void;
}) {
  const auth = useAuth();
  const [{ data }] = useQuery({ query: LocationOptionsDocument });
  const locations = (data?.tenant?.locationsList ?? []).filter(
    (x) => auth.isAdmin || x.showInLists || x.id === value,
  );
  const selected = locations.find((x) => x.id === value);

  return (
    <DropdownMenu>
      <DropdownMenuTrigger className={buttonCls({ variant: 'outline', size: 'sm' })}>
        <MapPin />
        {selected ? selected.name : 'Místa'}
      </DropdownMenuTrigger>
      <DropdownMenuContent>
        <DropdownMenuButton onSelect={() => onChange(null)}>
          {value ? <Circle /> : <CheckCircle2 />}
          Všechna místa
        </DropdownMenuButton>
        {locations.map((location) => (
          <DropdownMenuButton key={location.id} onSelect={() => onChange(location.id)}>
            {value === location.id ? <CheckCircle2 /> : <Circle />}
            {location.name}
          </DropdownMenuButton>
        ))}
      </DropdownMenuContent>
    </DropdownMenu>
  );
}
