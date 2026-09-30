import type { AddressDomain } from '@/graphql';
import { ExternalLink, MapPin } from 'lucide-react';

export function LocationAddress({ address, latitude, longitude, showMissingAddress = false }: {
  address: AddressDomain | null;
  latitude: number | null;
  longitude: number | null;
  showMissingAddress?: boolean;
}) {
  const number = [address?.conscriptionNumber, address?.orientationNumber]
    .filter(Boolean)
    .join('/');
  const street = [address?.street, number].filter(Boolean).join(' ');
  const city = [address?.postalCode, address?.city].filter(Boolean).join(' ');
  const mapQuery = latitude != null && longitude != null
    ? `${latitude},${longitude}`
    : [street, address?.district, city, address?.region]
        .filter(Boolean)
        .join(', ');

  if (!mapQuery) {
    return showMissingAddress ? <p className="text-sm text-neutral-10">(Adresa nevyplněna)</p> : null;
  }

  return (
    <div className="mb-4 flex items-start gap-2 text-sm">
      <MapPin className="mt-0.5 size-4 shrink-0 text-accent-11" aria-hidden="true" />
      <address className="not-italic">
        {street && <div>{street}</div>}
        {address?.district && <div>{address.district}</div>}
        {city && <div>{city}</div>}
        {address?.region && <div>{address.region}</div>}
        <a
          className="mt-1 inline-flex items-center gap-1 underline"
          href={`https://www.google.com/maps/search/?api=1&query=${encodeURIComponent(mapQuery)}`}
          target="_blank"
          rel="noreferrer"
        >
          Otevřít na mapě
          <ExternalLink className="size-3" aria-hidden="true" />
        </a>
      </address>
    </div>
  );
}
