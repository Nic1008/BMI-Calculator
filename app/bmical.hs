import System.IO
import Control.Monad (when)

-- BMI calculation function with height in centimeters
calculateBMI :: Double -> Double -> Double
calculateBMI weight heightCm = 
    let heightM = heightCm / 100.0  -- Convert cm to meters
    in weight / (heightM * heightM)

-- Function to calculate ideal weight range based on height and healthy BMI range (18.5-24.9)
calculateIdealWeightRange :: Double -> (Double, Double)
calculateIdealWeightRange heightCm =
    let heightM = heightCm / 100.0  -- Convert cm to meters
        minWeight = 18.5 * (heightM * heightM)
        maxWeight = 24.9 * (heightM * heightM)
    in (roundToOneDecimal minWeight, roundToOneDecimal maxWeight)

-- Function to round to one decimal place
roundToOneDecimal :: Double -> Double
roundToOneDecimal x = fromIntegral (round (x * 10)) / 10.0

-- Function to determine BMI category with detailed information
getBMICategory :: Double -> (String, String)
getBMICategory bmi
    | bmi < 18.5 = ("Underweight", "You may need to gain some weight. Consider consulting a healthcare provider.")
    | bmi < 25.0 = ("Normal weight", "You're at a healthy weight. Keep maintaining a balanced diet and regular exercise.")
    | bmi < 30.0 = ("Overweight", "You may benefit from some weight loss. Focus on healthy eating and regular physical activity.")
    | otherwise  = ("Obese", "It's recommended to consult with healthcare providers about weight management strategies.")

-- Display menu function
displayMenu :: IO ()
displayMenu = do
    putStrLn "\n=== BMI Calculator Menu ==="
    putStrLn "1. Calculate BMI"
    putStrLn "2. Calculate Ideal Weight"
    putStrLn "3. Show BMI Information"
    putStrLn "4. Exit"
    putStrLn "Enter your choice (1-4):"

-- Function to show BMI information
showBMIInfo :: IO ()
showBMIInfo = do
    putStrLn "\n=== BMI Categories and Information ==="
    putStrLn "BMI Categories:"
    putStrLn "* Underweight (BMI < 18.5)"
    putStrLn "  - May indicate malnutrition or other health issues"
    putStrLn "  - Higher risk for certain health problems"
    putStrLn "\n* Normal Weight (BMI 18.5-24.9)"
    putStrLn "  - Associated with the lowest health risks"
    putStrLn "  - Ideal range for most adults"
    putStrLn "\n* Overweight (BMI 25.0-29.9)"
    putStrLn "  - Increased risk for heart disease and diabetes"
    putStrLn "  - May benefit from lifestyle modifications"
    putStrLn "\n* Obese (BMI ≥ 30)"
    putStrLn "  - Higher risk for multiple health conditions"
    putStrLn "  - Medical consultation recommended"
    putStrLn "\nPress Enter to return to menu"
    _ <- getLine
    return ()

-- New function to calculate ideal weight only
calculateIdealWeightOnly :: IO ()
calculateIdealWeightOnly = do
    putStrLn "\n=== Ideal Weight Calculator ==="
    putStrLn "Enter your height in centimeters:"
    heightStr <- getLine
    
    let height = read heightStr :: Double
        (minIdealWeight, maxIdealWeight) = calculateIdealWeightRange height
    
    putStrLn $ "\nFor your height of " ++ show height ++ " cm:"
    putStrLn $ "Ideal Weight Range:"
    putStrLn $ "- Minimum healthy weight: " ++ show minIdealWeight ++ " kg"
    putStrLn $ "- Maximum healthy weight: " ++ show maxIdealWeight ++ " kg"
    putStrLn $ "\nThis range corresponds to a healthy BMI (18.5-24.9)"
    putStrLn "\nPress Enter to return to menu"
    _ <- getLine
    return ()

-- Function to calculate BMI from user input
calculateBMIFromInput :: IO ()
calculateBMIFromInput = do
    putStrLn "\n=== BMI Calculator ==="
    putStrLn "Enter your weight in kilograms:"
    weightStr <- getLine
    putStrLn "Enter your height in centimeters:"
    heightStr <- getLine
    
    let weight = read weightStr :: Double
        height = read heightStr :: Double
        bmi = calculateBMI weight height
        roundedBMI = roundToOneDecimal bmi
        (category, advice) = getBMICategory bmi
        (minIdealWeight, maxIdealWeight) = calculateIdealWeightRange height
    
    putStrLn $ "\nYour BMI is: " ++ show roundedBMI
    putStrLn $ "Category: " ++ category
    putStrLn $ "Advice: " ++ advice
    putStrLn $ "\nIdeal Weight Range for your height:"
    putStrLn $ "Minimum healthy weight: " ++ show minIdealWeight ++ " kg"
    putStrLn $ "Maximum healthy weight: " ++ show maxIdealWeight ++ " kg"
    
    -- Calculate weight difference from ideal range
    let weightDiff = if weight < minIdealWeight
                     then "You may need to gain " ++ show (roundToOneDecimal (minIdealWeight - weight)) ++ " kg to reach minimum healthy weight"
                     else if weight > maxIdealWeight
                          then "You may need to lose " ++ show (roundToOneDecimal (weight - maxIdealWeight)) ++ " kg to reach maximum healthy weight"
                          else "Your weight is within the healthy range!"
    
    putStrLn $ "\nWeight Status: " ++ weightDiff
    putStrLn "\nPress Enter to return to menu"
    _ <- getLine
    return ()

-- Main program loop
mainLoop :: IO ()
mainLoop = do
    displayMenu
    choice <- getLine
    case choice of
        "1" -> calculateBMIFromInput >> mainLoop
        "2" -> calculateIdealWeightOnly >> mainLoop
        "3" -> showBMIInfo >> mainLoop
        "4" -> putStrLn "Thank you for using the BMI Calculator!"
        _   -> putStrLn "Invalid choice. Please try again." >> mainLoop

-- Main function
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "Welcome to the BMI Calculator!"
    mainLoop