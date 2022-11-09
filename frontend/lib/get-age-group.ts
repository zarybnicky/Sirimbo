export function getAgeGroup(birthYear: number) {
  const diff = new Date().getFullYear() - birthYear;
  if (diff < 8) {
    return 'Do 8 let';
  } else if (diff < 10) {
    return 'Děti I';
  } else if (10 <= diff && diff < 12) {
    return 'Děti II';
  } else if (12 <= diff && diff < 14) {
    return 'Junioři I';
  } else if (14 <= diff && diff < 16) {
    return 'Junioři II';
  } else if (16 <= diff && diff < 19) {
    return 'Mládež';
  } else if (19 <= diff && diff < 21) {
    return 'Do 21 let';
  } else if (21 <= diff && diff < 35) {
    return 'Dospělí';
  } else if (35 <= diff && diff < 45) {
    return 'Senioři I';
  } else if (45 <= diff && diff < 55) {
    return 'Senioři II';
  } else if (55 <= diff && diff < 65) {
    return 'Senioři III';
  } else {
    return 'Senioři IV';
  }
}
