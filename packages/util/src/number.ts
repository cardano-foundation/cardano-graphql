export const clamp = (num: number, min: number, max: number) =>
  num <= min
    ? min
    : num >= max
      ? max
      : num
