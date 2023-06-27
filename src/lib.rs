use bigdecimal::Zero;
use lazy_static::lazy_static;
use num_integer::Integer;
use std::str::FromStr;

lazy_static! {
    // Use `cargo test -- --nocapture` to print on successful tests
    static ref TRACE: bool = std::env::var("TRACE").unwrap_or("".to_string()) != "";
}

macro_rules! trace {
    ($($arg:tt)*) => {
        if *TRACE {
            println!($($arg)*)
        }
    };
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct BigDecimal(bigdecimal::BigDecimal);

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct BigInt(num_bigint::BigInt);

impl FromStr for BigInt {
    type Err = <num_bigint::BigInt as FromStr>::Err;

    fn from_str(s: &str) -> Result<BigInt, Self::Err> {
        num_bigint::BigInt::from_str(s).map(BigInt)
    }
}

impl From<bigdecimal::BigDecimal> for BigDecimal {
    fn from(big_decimal: bigdecimal::BigDecimal) -> Self {
        BigDecimal(big_decimal).normalized()
    }
}

impl FromStr for BigDecimal {
    type Err = <bigdecimal::BigDecimal as FromStr>::Err;

    fn from_str(s: &str) -> Result<BigDecimal, Self::Err> {
        Ok(Self::from(bigdecimal::BigDecimal::from_str(s)?))
    }
}

impl BigDecimal {
    pub const MAX_SIGNFICANT_DIGITS: i32 = 34;

    pub fn as_bigint_and_exponent(&self) -> (num_bigint::BigInt, i64) {
        self.0.as_bigint_and_exponent()
    }

    pub fn zero() -> BigDecimal {
        BigDecimal(bigdecimal::BigDecimal::zero())
    }

    pub fn normalized(&self) -> BigDecimal {
        if self == &BigDecimal::zero() {
            return BigDecimal::zero();
        }

        // Round to the maximum significant digits.
        let big_decimal = if *TRACE {
            self.with_prec(Self::MAX_SIGNFICANT_DIGITS as u64)
        } else {
            self.0.with_prec(Self::MAX_SIGNFICANT_DIGITS as u64)
        };

        // let big_decimal = self.0.with_prec(Self::MAX_SIGNFICANT_DIGITS as u64);
        // BigDecimal(big_decimal)

        let (bigint, exp) = big_decimal.as_bigint_and_exponent();
        trace!(
            "normalized: as_bigint_and_exponent (bigint {}, exp {})",
            bigint,
            exp
        );

        let (sign, mut digits) = bigint.to_radix_be(10);
        trace!(
            "normalized: to_radix_be (sign {:?}, digits (str) {:?})",
            sign,
            digits_to_string(&digits)
        );

        let trailing_count = digits.iter().rev().take_while(|i| **i == 0).count();
        trace!("normalized: trailing_count {}", trailing_count);

        digits.truncate(digits.len() - trailing_count);
        trace!("normalized: digits truncated {}", digits_to_string(&digits));

        let int_val = num_bigint::BigInt::from_radix_be(sign, &digits, 10).unwrap();
        trace!("normalized: int_val {}", int_val);

        let scale = exp - trailing_count as i64;
        trace!("normalized: scale {}", scale);

        BigDecimal(bigdecimal::BigDecimal::new(int_val, scale))
    }

    // Port of bigdecimal::BigDecimal.with_prec but with adding tracing.
    // Good idea to keep for testing purposes, in which case, you should ensure
    // the implementation within BigDecimal did not change since the time we made
    // our own copy here.
    fn with_prec(&self, prec: u64) -> bigdecimal::BigDecimal {
        let (self_int_val, self_scale) = self.0.as_bigint_and_exponent();
        let digits = self.digits();
        trace!("with_prec: digits {}", digits);

        if digits > prec {
            trace!("with_prec: digits > prec");

            let diff = digits - prec;
            let p = ten_to_the(diff);
            let (mut q, r) = self_int_val.div_rem(&p);
            trace!("with_prec: digits > prec (q {}, r {})", q, r);

            // check for "leading zero" in remainder term; otherwise round
            if p < 10 * &r {
                let rounding_term = self.get_rounding_term(&r);
                q += self.get_rounding_term(&r);
                trace!(
                    "with_prec: digits > prec adding rounding term {}",
                    rounding_term,
                );
            }

            trace!(
                "with_prec: digits > prec got (bigint {}, exp {})",
                q,
                self_scale - diff as i64
            );

            bigdecimal::BigDecimal::new(q, self_scale - diff as i64)
        } else if digits < prec {
            trace!("with_prec: digits < prec");

            let diff = prec - digits;

            trace!(
                "with_prec: digits < prec prec got (bigint {}, exp {})",
                &self_int_val * ten_to_the(diff),
                self_scale + diff as i64
            );

            bigdecimal::BigDecimal::new(&self_int_val * ten_to_the(diff), self_scale + diff as i64)
        } else {
            trace!("with_prec: digits == prec");

            self.0.clone()
        }
    }

    fn digits(&self) -> u64 {
        let (int, _) = &self.0.as_bigint_and_exponent();

        if int.is_zero() {
            return 1;
        }
        trace!("digits: bits {}", int.bits());

        // guess number of digits based on number of bits in UInt
        let mut digits = (int.bits() as f64 / 3.3219280949) as u64;
        trace!("digits: guess digits {}", digits);

        let mut num = ten_to_the(digits);
        trace!("digits: num {}", num);

        while int >= &num {
            num *= 10u8;
            digits += 1;
            trace!("digits: add one digit");
        }

        trace!("digits: final digits {}", digits);
        digits
    }

    fn get_rounding_term(&self, num: &num_bigint::BigInt) -> u8 {
        if num.is_zero() {
            return 0;
        }

        let digits = (num.bits() as f64 / 3.3219280949) as u64;
        let mut n = ten_to_the(digits);

        // loop-method
        loop {
            if num < &n {
                return 1;
            }
            n *= 5;
            if num < &n {
                return 0;
            }
            n *= 2;
        }

        // string-method
        // let s = format!("{}", num);
        // let high_digit = u8::from_str(&s[0..1]).unwrap();
        // if high_digit < 5 { 0 } else { 1 }
    }
}

fn ten_to_the(pow: u64) -> num_bigint::BigInt {
    if pow < 20 {
        num_bigint::BigInt::from(10u64.pow(pow as u32))
    } else {
        let (half, rem) = pow.div_rem(&16);

        let mut x = ten_to_the(half);

        for _ in 0..4 {
            x = &x * &x;
        }

        if rem == 0 {
            x
        } else {
            x * ten_to_the(rem)
        }
    }
}

fn digits_to_string(digits: &Vec<u8>) -> String {
    digits
        .iter()
        .map(|x| x.to_string())
        .collect::<Vec<_>>()
        .join("")
}

#[cfg(test)]
mod tests {
    use stable_hash::{FieldAddress, StableHash, StableHasher};

    use super::*;
    use std::str::FromStr;

    macro_rules! map(
        { $($key:expr => $value:expr),* } => {
            {
                let mut m = std::collections::HashMap::new();
                $(
                    m.insert($key, $value);
                )*
                m
            }
         };
    );

    #[test]
    fn normalized() {
        #[rustfmt::skip]
        let cases: Vec<(_, &str, i64)> = vec![
            ("0.1", "1", 1),
            ("0.0", "0", 0),
            ("-0.1", "-1", 1),
            ("198.98765544", "19898765544", 8),
            ("0.00000093937698", "93937698", 14),
            ("98765587998098786876.0", "98765587998098786876", 0),
            ("98765000000", "98765", -6),
            ("-98765000000", "-98765", -6),
            ("98765000000.1", "987650000001", 1),
            ("-98765000000.2", "-987650000002", 1),

            // Positive rounding outside max scale (34)
            ("0.1234567890123456789012345678901234", "1234567890123456789012345678901234",34),
            ("0.12345678901234567890123456789012344", "1234567890123456789012345678901234", 34),
            ("0.12345678901234567890123456789012345", "1234567890123456789012345678901235", 34),
            ("0.12345678901234567890123456789012346", "1234567890123456789012345678901235", 34),

            // Negative rounding outside max scale (34)
            ("-0.1234567890123456789012345678901234", "-1234567890123456789012345678901234", 34),
            ("-0.12345678901234567890123456789012344", "-12345678901234567890123456789012344", 35),
            ("-0.12345678901234567890123456789012345", "-12345678901234567890123456789012345", 35),
            ("-0.12345678901234567890123456789012346", "-12345678901234567890123456789012346", 35),

            // Normalize negative numbers have a bug where scale is actually MAX + 1
            ("-0.123456789012345678901234567890123424", "-12345678901234567890123456789012342", 35),
            ("-0.123456789012345678901234567890123425", "-12345678901234567890123456789012342", 35),
            ("-0.123456789012345678901234567890123426", "-12345678901234567890123456789012342", 35),

            // Showcasing rounding effects when max digits is split before/after dot
            ("12.123456789012345678901234567890124", "1212345678901234567890123456789012", 32),
            ("12.123456789012345678901234567890125", "1212345678901234567890123456789013", 32),
            ("12.123456789012345678901234567890126", "1212345678901234567890123456789013", 32),

            ("-12.1234567890123456789012345678901234", "-12123456789012345678901234567890123", 33),
            ("-12.1234567890123456789012345678901235", "-12123456789012345678901234567890123", 33),
            ("-12.1234567890123456789012345678901236", "-12123456789012345678901234567890123", 33),

            ("1234567890123.123456789012345678901834567890124", "1234567890123123456789012345678902", 21),
            ("-1234567890123.123456789012345678901894567890124", "-12345678901231234567890123456789018", 22),

            // Showcasing rounding effects when max digits is all before dot
            ("1234567890123456789012345678901234", "1234567890123456789012345678901234", 0),
            ("12345678901234567890123456789012344", "1234567890123456789012345678901234", -1),
            ("12345678901234567890123456789012345", "1234567890123456789012345678901235", -1),
            ("12345678901234567890123456789012346", "1234567890123456789012345678901235", -1),

            ("-12345678901234567890123456789012345", "-12345678901234567890123456789012345", 0),
            ("-123456789012345678901234567890123454", "-12345678901234567890123456789012345", -1),
            ("-123456789012345678901234567890123455", "-12345678901234567890123456789012345", -1),
            ("-123456789012345678901234567890123456", "-12345678901234567890123456789012345", -1),

            ("10000000000000000000000000000000000000000", "1", -40),
            ("100000000000000000000000000000000000000001", "1", -41),

            ("19999999999999999999999999999999994", "1999999999999999999999999999999999", -1),
            ("19999999999999999999999999999999995", "2", -34),
            ("19999999999999999999999999999999985", "1999999999999999999999999999999999", -1),

            ("1999999999999999999999999999999999", "1999999999999999999999999999999999", 0),
            ("199999999999999999999999999999999", "199999999999999999999999999999999", 0),
            ("19999999999999999999999999999999999", "2", -34),
            ("199999999999999999999999999999999999999999", "2", -41),

            ("1444444444444444444444444444444444", "1444444444444444444444444444444444", 0),
            ("14444444444444444444444444444444444", "1444444444444444444444444444444444", -1),
            ("144444444444444444444444444444444444", "1444444444444444444444444444444444", -2),

            ("1555555555555555555555555555555555", "1555555555555555555555555555555555", 0),
            ("15555555555555555555555555555555555", "1555555555555555555555555555555556", -1),
            ("155555555555555555555555555555555555", "1555555555555555555555555555555556", -2),

            ("0.001555727668872877713264010710964539", "1555727668872877713264010710964539", 36),
        ];

        for (input, expected_int, expected_scale) in cases.into_iter() {
            let dec = BigDecimal::from_str(input).unwrap();
            let (actual_int, actual_scale) = dec.as_bigint_and_exponent();

            assert!(
                BigInt::from_str(expected_int).unwrap().0 == actual_int
                    && actual_scale == expected_scale,
                "normalized {} (int [expected {}, actual {}], scale [expected {}, actual {}])",
                input,
                BigInt::from_str(expected_int).unwrap().0,
                actual_int,
                expected_scale,
                actual_scale,
            );
        }
    }

    #[test]
    fn unnormalized() {
        #[rustfmt::skip]
        let cases: Vec<(_, &str, i64)> = vec![
            ("0.1", "1", 1),
            ("0.0", "0", 1),
            ("-0.1", "-1", 1),
            ("198.98765544", "19898765544", 8),
            ("0.00000093937698", "93937698", 14),
            ("98765587998098786876.0", "987655879980987868760", 1),
            ("98765000000", "98765000000", 0),
            ("-98765000000", "-98765000000", 0),
            ("98765000000.1", "987650000001", 1),
            ("-98765000000.2", "-987650000002", 1),

            // Positive rounding outside max scale (34)
            ("0.1234567890123456789012345678901234", "1234567890123456789012345678901234",34),
            ("0.12345678901234567890123456789012344", "12345678901234567890123456789012344", 35),
            ("0.12345678901234567890123456789012345", "12345678901234567890123456789012345", 35),
            ("0.12345678901234567890123456789012346", "12345678901234567890123456789012346", 35),

            // Negative rounding outside max scale (34)
            ("-0.1234567890123456789012345678901234", "-1234567890123456789012345678901234", 34),
            ("-0.12345678901234567890123456789012344", "-12345678901234567890123456789012344", 35),
            ("-0.12345678901234567890123456789012345", "-12345678901234567890123456789012345", 35),
            ("-0.12345678901234567890123456789012346", "-12345678901234567890123456789012346", 35),

            // Normalize negative numbers have a bug where scale is actually MAX + 1
            ("-0.123456789012345678901234567890123424", "-123456789012345678901234567890123424", 36),
            ("-0.123456789012345678901234567890123425", "-123456789012345678901234567890123425", 36),
            ("-0.123456789012345678901234567890123426", "-123456789012345678901234567890123426", 36),

            // Showcasing rounding effects when max digits is split before/after dot
            ("12.123456789012345678901234567890124", "12123456789012345678901234567890124", 33),
            ("12.123456789012345678901234567890125", "12123456789012345678901234567890125", 33),
            ("12.123456789012345678901234567890126", "12123456789012345678901234567890126", 33),

            ("-12.1234567890123456789012345678901234", "-121234567890123456789012345678901234", 34),
            ("-12.1234567890123456789012345678901235", "-121234567890123456789012345678901235", 34),
            ("-12.1234567890123456789012345678901236", "-121234567890123456789012345678901236", 34),

            ("1234567890123.123456789012345678901834567890124", "1234567890123123456789012345678901834567890124", 33),
            ("-1234567890123.123456789012345678901894567890124", "-1234567890123123456789012345678901894567890124", 33),

            // Showcasing rounding effects when max digits is all before dot
            ("1234567890123456789012345678901234", "1234567890123456789012345678901234", 0),
            ("12345678901234567890123456789012344", "12345678901234567890123456789012344", 0),
            ("12345678901234567890123456789012345", "12345678901234567890123456789012345", 0),
            ("12345678901234567890123456789012345", "12345678901234567890123456789012345", 0),

            ("-12345678901234567890123456789012345", "-12345678901234567890123456789012345", 0),
            ("-123456789012345678901234567890123454", "-123456789012345678901234567890123454", 0),
            ("-123456789012345678901234567890123455", "-123456789012345678901234567890123455", 0),
            ("-123456789012345678901234567890123456", "-123456789012345678901234567890123456", 0),

            ("10000000000000000000000000000000000000000", "10000000000000000000000000000000000000000", 0),
            ("100000000000000000000000000000000000000001", "100000000000000000000000000000000000000001", 0),

            ("19999999999999999999999999999999994", "19999999999999999999999999999999994", 0),
            ("19999999999999999999999999999999995", "19999999999999999999999999999999995", 0),
            ("19999999999999999999999999999999985", "19999999999999999999999999999999985", 0),

            ("1999999999999999999999999999999999", "1999999999999999999999999999999999", 0),
            ("199999999999999999999999999999999", "199999999999999999999999999999999", 0),
            ("19999999999999999999999999999999999", "19999999999999999999999999999999999", 0),
            ("199999999999999999999999999999999999999999", "199999999999999999999999999999999999999999", 0),

            ("1444444444444444444444444444444444", "1444444444444444444444444444444444", 0),
            ("14444444444444444444444444444444444", "14444444444444444444444444444444444", 0),
            ("144444444444444444444444444444444444", "144444444444444444444444444444444444", 0),

            ("1555555555555555555555555555555555", "1555555555555555555555555555555555", 0),
            ("15555555555555555555555555555555555", "15555555555555555555555555555555555", 0),
            ("155555555555555555555555555555555555", "155555555555555555555555555555555555", 0),
        ];

        for (input, expected_int, expected_scale) in cases.into_iter() {
            let dec = bigdecimal::BigDecimal::from_str(&input).unwrap();
            let (actual_int, actual_scale) = dec.as_bigint_and_exponent();

            assert!(
                BigInt::from_str(expected_int).unwrap().0 == actual_int
                    && expected_scale == actual_scale,
                "unnormalized {} (int [expected {}, actual {}], scale [expected {}, actual {}])",
                input,
                BigInt::from_str(expected_int).unwrap().0,
                actual_int,
                expected_scale,
                actual_scale,
            );
        }
    }

    #[test]
    fn unnormalized_but_with_precision() {
        #[rustfmt::skip]
        let cases: Vec<(_, &str, i64)> = vec![
            ("0.1", "1000000000000000000000000000000000", 34),
            ("0.0", "0", 34),
            ("-0.1", "-10000000000000000000000000000000000", 35),
            ("198.98765544", "1989876554400000000000000000000000", 31),
            ("0.00000093937698", "9393769800000000000000000000000000", 40),
            ("98765587998098786876.0", "9876558799809878687600000000000000", 14),
            ("98765000000", "9876500000000000000000000000000000", 23),
            ("-98765000000", "-9876500000000000000000000000000000", 23),
            ("98765000000.1", "9876500000010000000000000000000000", 23),
            ("-98765000000.2", "-9876500000020000000000000000000000", 23),

            // Positive rounding outside max scale (34)
            ("0.1234567890123456789012345678901234", "1234567890123456789012345678901234",34),
            ("0.12345678901234567890123456789012344", "1234567890123456789012345678901234", 34),
            ("0.12345678901234567890123456789012345", "1234567890123456789012345678901235", 34),
            ("0.12345678901234567890123456789012346", "1234567890123456789012345678901235", 34),

            // Negative rounding outside max scale (34)
            ("-0.1234567890123456789012345678901234", "-12345678901234567890123456789012340", 35),
            ("-0.12345678901234567890123456789012344", "-12345678901234567890123456789012344", 35),
            ("-0.12345678901234567890123456789012345", "-12345678901234567890123456789012345", 35),
            ("-0.12345678901234567890123456789012346", "-12345678901234567890123456789012346", 35),

            // Normalize negative numbers have a bug where scale is actually MAX + 1
            ("-0.123456789012345678901234567890123424", "-12345678901234567890123456789012342", 35),
            ("-0.123456789012345678901234567890123425", "-12345678901234567890123456789012342", 35),
            ("-0.123456789012345678901234567890123426", "-12345678901234567890123456789012342", 35),

            // Showcasing rounding effects when max digits is split before/after dot
            ("12.123456789012345678901234567890124", "1212345678901234567890123456789012", 32),
            ("12.123456789012345678901234567890125", "1212345678901234567890123456789013", 32),
            ("12.123456789012345678901234567890126", "1212345678901234567890123456789013", 32),

            ("-12.1234567890123456789012345678901234", "-12123456789012345678901234567890123", 33),
            ("-12.1234567890123456789012345678901235", "-12123456789012345678901234567890123", 33),
            ("-12.1234567890123456789012345678901236", "-12123456789012345678901234567890123", 33),

            ("1234567890123.123456789012345678901834567890124", "1234567890123123456789012345678902", 21),
            ("-1234567890123.123456789012345678901894567890124", "-12345678901231234567890123456789018", 22),

            // Showcasing rounding effects when max digits is all before dot
            ("1234567890123456789012345678901234", "1234567890123456789012345678901234", 0),
            ("12345678901234567890123456789012344", "1234567890123456789012345678901234", -1),
            ("12345678901234567890123456789012345", "1234567890123456789012345678901235", -1),
            ("12345678901234567890123456789012346", "1234567890123456789012345678901235", -1),

            ("-12345678901234567890123456789012345", "-12345678901234567890123456789012345", 0),
            ("-123456789012345678901234567890123454", "-12345678901234567890123456789012345", -1),
            ("-123456789012345678901234567890123455", "-12345678901234567890123456789012345", -1),
            ("-123456789012345678901234567890123456", "-12345678901234567890123456789012345", -1),

            ("10000000000000000000000000000000000000000", "1000000000000000000000000000000000", -7),
            ("100000000000000000000000000000000000000001", "1000000000000000000000000000000000", -8),

            ("19999999999999999999999999999999994", "1999999999999999999999999999999999", -1),
            ("19999999999999999999999999999999995", "2000000000000000000000000000000000", -1),
            ("19999999999999999999999999999999985", "1999999999999999999999999999999999", -1),

            ("1999999999999999999999999999999999", "1999999999999999999999999999999999", 0),
            ("199999999999999999999999999999999", "1999999999999999999999999999999990",1),
            ("19999999999999999999999999999999999", "2000000000000000000000000000000000", -1),
            ("199999999999999999999999999999999999999999", "2000000000000000000000000000000000", -8),

            ("1444444444444444444444444444444444", "1444444444444444444444444444444444", 0),
            ("14444444444444444444444444444444444", "1444444444444444444444444444444444", -1),
            ("144444444444444444444444444444444444", "1444444444444444444444444444444444", -2),

            ("1555555555555555555555555555555555", "1555555555555555555555555555555555", 0),
            ("15555555555555555555555555555555555", "1555555555555555555555555555555556", -1),
            ("155555555555555555555555555555555555", "1555555555555555555555555555555556", -2),

            ("155555555555555555555555555555555555", "1555555555555555555555555555555556", -2),

            ("0.001555727668872877713264010710964539", "1555727668872877713264010710964539", 36),
        ];

        for (input, expected_int, expected_scale) in cases.into_iter() {
            let dec = bigdecimal::BigDecimal::from_str(&input)
                .unwrap()
                .with_prec(34);
            let (actual_int, actual_scale) = dec.as_bigint_and_exponent();

            assert!(
                BigInt::from_str(expected_int).unwrap().0 == actual_int
                    && expected_scale == actual_scale,
                "unnormalized (with prec) {} (int [expected {}, actual {}], scale [expected {}, actual {}])",
                input,
                BigInt::from_str(expected_int).unwrap().0,
                actual_int,
                expected_scale,
                actual_scale,
            );
        }
    }

    #[test]
    fn stable_hash_string() {
        assert_eq!(
            stable_hash::fast_stable_hash(&"1000000000000000000000000000000000"),
            82550105226381465442150169023502768996
        );
    }

    #[test]
    fn stable_has_list() {
        assert_eq!(
            stable_hash::fast_stable_hash(&Vec::<u8>::new()),
            320514965852340112707580934281173047643
        );

        assert_eq!(
            stable_hash::fast_stable_hash(&vec![0]),
            135263302447443856369810803691068577694
        );

        assert_eq!(
            stable_hash::fast_stable_hash(&vec![1]),
            181745098936733907021518655505145702128
        );

        assert_eq!(
            stable_hash::fast_stable_hash(&vec![0, 1, 3]),
            227549997251239301319289036454140551565
        );

        assert_eq!(
            stable_hash::fast_stable_hash(&vec![3, 0, 1]),
            318064286550914597684751961019563608459
        );
    }

    #[test]
    fn stable_hash_map_eq() {
        let m1 = map! { 1u32 => "one", 2u32 => "two", 3u32 => "three" };
        let m2 = map! { 3u32 => "three", 1u32 => "one", 2u32 => "two" };

        assert_eq!(
            60093794751952876589018848897648863192,
            stable_hash::fast_stable_hash(&m1)
        );

        assert_eq!(
            60093794751952876589018848897648863192,
            stable_hash::fast_stable_hash(&m2)
        );
    }

    #[test]
    fn stable_hash_map_ne_count() {
        let m1 = map! { 1 => "one", 2 => "two", 3 => "three", 0 => "" };
        let m2 = map! { 1 => "one", 2 => "two", 3 => "three" };

        assert!(stable_hash::fast_stable_hash(&m1) != stable_hash::fast_stable_hash(&m2));
    }

    #[test]
    fn stable_hash_map_ne_key() {
        let m1 = map! { 9 => "one", 2 => "two", 3 => "three" };
        let m2 = map! { 1 => "one", 2 => "two", 3 => "three" };

        assert!(stable_hash::fast_stable_hash(&m1) != stable_hash::fast_stable_hash(&m2));
    }

    #[test]
    fn stable_hash_map_ne_value() {
        let m1 = map! { 1 => "X", 2 => "two", 3 => "three" };
        let m2 = map! { 1 => "one", 2 => "two", 3 => "three" };

        assert!(stable_hash::fast_stable_hash(&m1) != stable_hash::fast_stable_hash(&m2));
    }

    #[test]
    fn stable_hash_map_ne_swap() {
        let m1 = map! { 1 => "one", 2 => "two" };
        let m2 = map! { 1 => "two", 2 => "one" };

        assert!(stable_hash::fast_stable_hash(&m1) != stable_hash::fast_stable_hash(&m2));
    }

    #[test]
    fn stable_hash_int_i8() {
        assert_eq!(
            2575436948546927940500443723565624388,
            stable_hash::fast_stable_hash(&-256i16)
        );

        assert_eq!(
            201589876719799452230445857493583317400,
            stable_hash::fast_stable_hash(&256i16)
        );
    }

    #[test]
    fn stable_hash_int_u8() {
        assert_eq!(
            263946226580928315975306067326554590217,
            stable_hash::fast_stable_hash(&8u8)
        );

        assert_eq!(
            182395296116387546137591220945749437249,
            stable_hash::fast_stable_hash(&255u8)
        );
    }

    struct DoubleChild;

    impl StableHash for DoubleChild {
        fn stable_hash<H: StableHasher>(&self, field_address: H::Addr, state: &mut H) {
            state.write(field_address.child(1), &[]);
            state.write(field_address.child(1), &[]);
        }
    }

    #[test]
    fn stable_hash_double_child() {
        assert_eq!(
            261232071512772414229682083989926651266,
            stable_hash::fast_stable_hash(&DoubleChild {})
        );
    }

    struct One<T0> {
        one: T0,
    }

    impl<T0: StableHash> StableHash for One<T0> {
        fn stable_hash<H: StableHasher>(&self, field_address: H::Addr, state: &mut H) {
            self.one.stable_hash(field_address.child(0), state);
        }
    }

    struct Two<T0, T1> {
        one: T0,
        two: T1,
    }

    impl<T0: StableHash, T1: StableHash> StableHash for Two<T0, T1> {
        fn stable_hash<H: StableHasher>(&self, field_address: H::Addr, state: &mut H) {
            self.one.stable_hash(field_address.child(0), state);
            self.two.stable_hash(field_address.child(1), state);
        }
    }

    #[test]
    fn stable_hash_add_optional_field() {
        let one = One { one: 5u32 };
        let two = Two {
            one: 5u32,
            two: Option::<u32>::None,
        };

        assert_eq!(
            102568403942768160221811810082933398928,
            stable_hash::fast_stable_hash(&one)
        );

        assert_eq!(
            102568403942768160221811810082933398928,
            stable_hash::fast_stable_hash(&two)
        );
    }

    #[test]
    fn stable_hash_add_default_field() {
        let one = One { one: "one" };
        let two = Two {
            one: "one",
            two: "",
        };

        assert_eq!(
            237994494046445339248193596542695086083,
            stable_hash::fast_stable_hash(&one)
        );
        assert_eq!(
            237994494046445339248193596542695086083,
            stable_hash::fast_stable_hash(&two)
        );
    }

    #[test]
    fn stable_hash_add_non_default_field() {
        let one = One { one: "one" };
        let two = Two {
            one: "one",
            two: "two",
        };

        assert!(stable_hash::fast_stable_hash(&one) != stable_hash::fast_stable_hash(&two));
    }
}
